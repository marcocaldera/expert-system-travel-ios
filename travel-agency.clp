;;;======================================================
;;;   Sistema esperto per agenzia viaggi
;;;
;;;     TRAGEX: TRavel AGency EXpert system.
;;;     Questo sistema vuole, a seguito di semplici
;;;     indicazioni fornite dall'utente, trovare un itinerario
;;;     adatto alla persona che interroga il sistema
;;;
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================


;;****************
;;* MODULO MAIN *
;;****************
(defmodule MAIN (export ?ALL))

;;;** FUNCTION MODULO MAIN **

(deffunction MAIN::ask-question (?question ?allowed-values)
    (printout t ?question);stampo la domanda
    (bind ?answer (read));leggo la risposta "read" dell'utente
    (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)));lexemep verifica la ?answer sia una stringa o un simbolo
    (while (and
                (not (member ?answer ?allowed-values));la risposta non appartiene a quelle consentite
                (not (and (integerp ?answer) (member number ?allowed-values)));la domanda accetta interi ma la risposta non lo è
            ) do
    
        (printout t ?question)
        (bind ?answer (read))
        (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
        
    )
    
    ?answer;return
)

(deffunction MAIN::atan2 (?y ?x)

    (if (> ?x 0) then (bind ?answer (atan (/ ?y ?x))))
    (if (and (< ?x 0) (>= ?y 0)) then (bind ?answer (+ (atan (/ ?y ?x)) (pi))))
    (if (and (< ?x 0) (< ?y 0)) then (bind ?answer (- (atan (/ ?y ?x)) (pi))))
    (if (and (= ?x 0) (> ?y 0)) then (bind ?answer (/ (pi) 2)))
    (if (and (= ?x 0) (< ?y 0)) then (bind ?answer (/ (pi) -2)))

    ?answer
)

(deffunction MAIN::km-distance (?lat1 ?lat2 ?lon1 ?lon2)

    (bind ?r 6371)
    (bind ?lat (deg-rad (- ?lat2 ?lat1)))
    (bind ?lon (deg-rad (- ?lon2 ?lon1)))

    (bind ?a (+ 
        (* 
            (sin (/ ?lat 2)) 
            (sin (/ ?lat 2))
        ) 
        (* 
            (* 
                (cos (deg-rad ?lat1))
                (cos (deg-rad ?lat2))
            )
            (* 
                (sin (/ ?lon 2))
                (sin (/ ?lon 2))
            )
        )
    ))
    (bind ?c (* 
        2
        (atan2 (sqrt ?a) (sqrt (- 1 ?a)))
    ))

    (bind ?d (* ?r ?c))

    ?d
)



;;;** TEMPLATE MODULO MAIN **

(deftemplate MAIN::travel-banchmark
    (slot name)
    (slot value)
    (slot certainty (type FLOAT) (range -1.0 +1.0) (default 1.0))
    (slot inferred (default TRUE))
)

;;;** REGOLE MODULO MAIN **

;Punto di partenza del sistema esperto
(defrule MAIN::start
    (declare (salience 10000)) ;Indico la salience massima possibile (10.000)
    =>
    (set-fact-duplication TRUE);Permette di avere fatti duplicati
    (focus QUESTIONS RULES LOCATION TRIP);Indico l'ordine dello stack focus
;    (printout t "È un buon inizio")
)


;Combina insieme i cf dei travel-banchmark che hanno stesso nome e value
(defrule MAIN::combine-certainties
  (declare (salience 100) (auto-focus TRUE))
  ?rem1 <- (travel-banchmark (name ?rel) (value ?val) (certainty ?c1))
  ?rem2 <- (travel-banchmark (name ?rel) (value ?val) (certainty ?c2))
  (test (neq ?rem1 ?rem2));neq = not EQuals to
  =>
  (retract ?rem1);cancella rem1
  
  (if (and (>= ?c1 0) (>= ?c2 0))
    then (modify ?rem2 (certainty (- (+ ?c1 ?c2) (* ?c1 ?c2))));(c1+c2)-(c1*c2)
    else (if (and (< ?c1 0) (< ?c2 0)) 
        then (modify ?rem2 (certainty (+ (+ ?c1 ?c2) (* ?c1 ?c2))));(c1+c2)+(c1*c2)
         else (modify ?rem2 (certainty (/ (+ ?c1 ?c2) (- 1 (min (abs ?c1) (abs ?c2))))));(c1+c2)/(1-min(|c1|,|c2|))
        )
    )
)

; (deffacts MAIN::test-fact
;     (travel-banchmark (name tourism-type) (value balneare) (certainty 0.7))
;     (travel-banchmark (name tourism-type) (value montano) (certainty 0.4))
;     (travel-banchmark (name tourism-type) (value sportivo) (certainty 0.8))
;     (travel-banchmark (name tourism-type) (value enogastronomico) (certainty 0.2))
;     ; (travel-banchmark (name min-resort-star) (value 3) (certainty 0.7))
;     ; (travel-banchmark (name travel-region) (value piemonte) (certainty 0.7))
;     ; (travel-banchmark (name travel-region) (value toscana) (certainty 0.4))
;     ; (travel-banchmark (name travel-region) (value liguria) (certainty 0.6))
;     ; (travel-banchmark (name travel-region) (value puglia) (certainty 0.1))
; )


;;****************
;;* MODULO LOCATION *
;;****************
(defmodule LOCATION (import MAIN ?ALL) (export ?ALL))

;Località turistiche presenti sul territorio italiano
(deftemplate LOCATION::place
    (slot name (default ?NONE));?NONE indica che il campo name è obbligatorio
    (slot region)
    (slot ID)
    (multislot coordinates (cardinality 2 2))
)

(deftemplate LOCATION::tourism-type
    (slot place-ID)
    (slot type)
    (slot score)
)

;Alberghi presenti sul territorio italiano
(deftemplate LOCATION::resort
    (slot name (default ?NONE));?NONE indica che il campo name è obbligatorio
    (slot star);stelle dell'albergo
    (slot rooms-number);numero di camere disponibili nell'hotel
    (slot place-ID);nome della località turistica
)

;;;** REGOLE MODULO LOCATION ***********************************************************************

;Regola che permette di definire il grado di approrpiatezza di una location rispetto ai banchmark inseriti dall'utente
;
;Escludo i resort con resort-start < di min-resort-star
;Escludo i resort in cui non ci possono stare tutte le persone del viaggio insieme
;Escludo i resort che non hanno tourism type in comune con i banchmark
;Declasso i resort che non sono nella fav-region (se questa è stata inserita)
(defrule LOCATION::select-location  
    (place 
        (name ?place-name)
        (region ?region)
        (ID ?id)
    )
    (tourism-type 
        (place-ID ?id)
        (type ?tourism-type)
        (score ?score)
    )
    (resort
        (name ?resort-name)
        (star ?resort-star)
        (place-ID ?id)
        (rooms-number ?rooms-number)
    )
    (travel-banchmark (name min-resort-star) (value ?min-resort-star&~unknown) (certainty ?c1))
    (travel-banchmark (name tourism-type) (value ?tourism-type) (certainty ?c2))
    (travel-banchmark (name favourite-region) (value ?fav-region) (certainty ?c3))
    (travel-banchmark (name people-number) (value ?people-number) (certainty ?c4))

    (test (>= ?resort-star ?min-resort-star))
    (test (>= (* ?rooms-number 2) ?people-number));Escludo i resort che non hanno abbastanza stanze
    =>
    ;se il place è in una regione non scelta facciamo un downgrade
    (if (and (neq ?fav-region ?region) (neq ?fav-region unknown))
        then (assert
                (travel-banchmark
                    (name location);creiamo un banchmark di tipo location
                    (value ?resort-name)
                    (certainty -0.3)
                )
            )
    )

    (assert
        (travel-banchmark
            (name location);creiamo un banchmark di tipo location
            (value ?resort-name)
            (certainty (min ?c1 ?c2 ?c3 ?c4 (/ ?score 5)));score/5 mi da il grado di certainty con cui il place appartiene ad un tourism-type
        )
    )

)


;;;** FATTI MODULO LOCATION ***********************************************************************

(deffacts LOCATION::place-list
    (place (name rapallo) (region liguria) (coordinates 44.352200 9.230800) (ID 1))
    (tourism-type (place-ID 1) (type balneare) (score 5))
    (tourism-type (place-ID 1) (type montano) (score 3))
    
    (place (name diano) (region liguria) (coordinates 43.909890 8.081830) (ID 2))
    (tourism-type (place-ID 2) (type balneare) (score 5))
    (tourism-type (place-ID 2) (type montano) (score 2))
    (tourism-type (place-ID 2) (type sportivo) (score 4))
    (tourism-type (place-ID 2) (type enogastronomico) (score 2))

    (place (name alassio) (region liguria) (coordinates 44.014336 8.181174) (ID 3))
    (tourism-type (place-ID 3) (type balneare) (score 5))
    (tourism-type (place-ID 3) (type montano) (score 2))

    (place (name cuneo) (region piemonte) (coordinates 44.384476 7.542671) (ID 4))
    (tourism-type (place-ID 4) (type balneare) (score 1))
    (tourism-type (place-ID 4) (type montano) (score 4))
)

(deffacts LOCATION::resort-list
    (resort (name resort-1a) (star 3) (rooms-number 9) (place-ID 1))
    (resort (name resort-1b) (star 2) (rooms-number 4) (place-ID 1))
    (resort (name resort-1c) (star 3) (rooms-number 6) (place-ID 1))
    (resort (name resort-1d) (star 3) (rooms-number 3) (place-ID 1))

    (resort (name resort-2a) (star 3) (rooms-number 8) (place-ID 2))
    (resort (name resort-2b) (star 2) (rooms-number 2) (place-ID 2))
    (resort (name resort-2c) (star 4) (rooms-number 5) (place-ID 2))
    (resort (name resort-2d) (star 3) (rooms-number 3) (place-ID 2))

    (resort (name resort-3a) (star 3) (rooms-number 3) (place-ID 3))
    (resort (name resort-3b) (star 2) (rooms-number 6) (place-ID 3))
    (resort (name resort-3a) (star 3) (rooms-number 2) (place-ID 3))
    (resort (name resort-3b) (star 3) (rooms-number 3) (place-ID 3))

    (resort (name resort-4a) (star 3) (rooms-number 4) (place-ID 4))
    (resort (name resort-4b) (star 3) (rooms-number 7) (place-ID 4))
    (resort (name resort-4a) (star 2) (rooms-number 6) (place-ID 4))
    (resort (name resort-4b) (star 4) (rooms-number 3) (place-ID 4))
)

;;****************
;;* MODULO TRIP *
;;
;; Modulo per gestire i viaggi
;;****************
(defmodule TRIP (import MAIN ?ALL) (import LOCATION ?ALL))

; (deftemplate TRIP::trip
;     (multislot resort-sequence)
; )

(deftemplate TRIP::link
    (slot resort1)
    (slot resort2)
    (slot distance)
)
(deftemplate TRIP::trip
   (multislot values)
)

(defrule first-in-permutation
    ; (travel-banchmark (name number-of-place) (value ?k))
   ; (k-combination ~0)
   (travel-banchmark (name location) (value ?city))
   =>
   (assert (trip (values ?city)))
)

(defrule next-in-permutation
    (travel-banchmark (name number-of-place) (value ?k))
   ; (k-combination ?k)
   ?p <- (trip (values $?cities))

   (test (< (length$ ?cities) ?k))
   (travel-banchmark (name location) (value ?city))

   (test (not (member$ ?city ?cities)))
   =>
   (assert (trip (values ?cities ?city)))
)

(defrule cleanup
   (declare (salience -5))
   (travel-banchmark (name number-of-place) (value ?k))
   ; (k-combination ?k)
   ?p <- (trip (values $?cities))
   (test (< (length$ ?cities) ?k))
   =>
   (retract ?p)
)


(defrule TRIP::good-link
    (travel-banchmark (name location) (value ?resort1))
    (travel-banchmark (name location) (value ?resort2))

    (resort (name ?resort1) (place-ID ?id1))
    (resort (name ?resort2) (place-ID ?id2))

    (place (ID ?id1) (coordinates ?lat1 ?lon1))
    (place (ID ?id1) (coordinates ?lat2 ?lon2))

    (test (neq ?resort1 ?resort2));non calcolo la distanza sullo stesso resort (ovviamente)
    =>
    (printout t ?resort1 ?lat1 ?resort2 ?lat2 crlf)
    (assert (link 
        (resort1 ?resort1)
        (resort2 ?resort2)
        (distance (km-distance ?lat1 ?lat2 ?lon1 ?lon2))
    ))

)

; (defrule TRIP::bho
;     ; (travel-banchmark (name travel-budget) (value ?budget))
;     ; (travel-banchmark (name travel-duration) (value ?duration))
;     ; (travel-banchmark (name people-number) (value ?people-number))

;     =>
;     (bind ?all-location (find-all-facts ((?f travel-banchmark)) (eq ?f:name location)))
;     ; (do-for-all-facts ((?f travel-banchmark)) (eq ?f:name location) (printout t ?f:value crlf))

;     ; (bind ?length (length ?all-location))
;     ; (printout t (length ?all-location) crlf)
;     ; (printout t (nth ?length ?all-location) crlf)

;     (foreach ?loc ?all-location
;       (assert (trip (resort-sequence ?loc)))
;     )

;     ; (loop-for-count (?all-location 1 ?length) do
;     ;     (assert (trip (resort-sequence )))
;     ;     ; (loop-for-count (?cnt2 1 3) do
;     ;     ;     (printout t ?cnt1 " " ?cnt2 crlf)
;     ;     ; )
;     ; )


;     ; (do-for-all-facts ((?f student)) TRUE
;     ;   (printout t ?f:name crlf)))
    
; )

;;****************
;;* MODULO QUESTIONS *
;;
;; Modulo per gestire le domande da porre all'utente
;;****************
(defmodule QUESTIONS (import MAIN ?ALL) (export ?ALL))

(deftemplate QUESTIONS::question
    (slot travel-banchmark (default ?NONE))
    (slot the-question (default ?NONE))
    (multislot valid-answers (default ?NONE))
    (slot already-asked (default FALSE));forse non serve
    (multislot precursors));possono anche non essercene
)


;;;** REGOLE MODULO QUESTIONS **

(defrule QUESTIONS::ask-a-question
    ?f <- (question
;            (already-asked FALSE) ;se non ho ancora fatto la domanda
            (precursors);se la domanda non ha precursori
            (the-question ?the-question)
            (travel-banchmark ?the-attribute)
            (valid-answers $?valid-answers)
    )
    =>
;    (modify ?f (already-asked TRUE)) ;indica che ho posto la domanda
    (assert
        (travel-banchmark
            (name ?the-attribute)
            (value (ask-question ?the-question ?valid-answers)) ;creo un nuovo attribute con la risposta alla domanda (dopo averla chiesta con ask-question
            (inferred FALSE)
        )
    )
)

;;;** FATTI MODULO QUESTIONS **

(deffacts QUESTIONS::question-list
    (question
        (travel-banchmark travel-duration)
        (the-question "Quanto deve durare il viaggio?")
        (valid-answers number);l'inserimento è obbligatorio
    )
    (question
        (travel-banchmark travel-budget)
        (the-question "Quanto sei disposto a spendere?")
        (valid-answers number);l'inserimento è obbligatorio
    )
    (question
        (travel-banchmark people-number)
        (the-question "Quante persone partecipano al viaggio?")
        (valid-answers number);l'inserimento è obbligatorio
    )
    (question
        (travel-banchmark min-resort-star)
        (the-question "Quante stelle deve almeno avere l'hotel?")
        (valid-answers number unknown)
    )
    (question
        (travel-banchmark number-of-place)
        (the-question "Quante mete vuoi visitare?")
        (valid-answers number unknown)
    )
    (question
        (travel-banchmark trip-type)
        (the-question "Preferisci una vacanza culturale o rilassante?")
        (valid-answers culturale rilassante unknown)
    )
    (question
        (travel-banchmark personal-trait)
        (the-question "Preferisci di più vivere l'avventura o la comodità?")
        (valid-answers avventura comodità unknown)
    )
    (question
        (travel-banchmark favourite-region)
        (the-question "Quale regione preferisci?")
        (valid-answers piemonte liguria unknown)
    )

    ;unknown
    ;altre domande: regione che si vuole visitare? (se la seleziona si fa in modo che non possano esserci rules che ne considerano altre) (magari lasciare la scelta solo tra 4-5 regioni)
    ;regione che non si vuole visitare? (certainty -1.0)
)

;;****************
;;* MODULO RULES *
;;
;;
;;****************
(defmodule RULES (import MAIN ?ALL))

;;;** TEMPLATE MODULO RULES **

(deftemplate RULES::rule
    (slot certainty (type FLOAT) (range -1.0 +1.0) (default 1.0))
    (multislot if)
    (multislot then)
)

;;;** REGOLE MODULO RULES ***********************************************************************

; cancella gli "and" che ci sono nei "then"
(defrule RULES::throw-away-ands-in-consequent
    ?f <- (rule (then and $?rest))
    =>
(modify ?f (then ?rest)))

;quando una condizone all'interno dell'if di una "rule" è soddisfatta la si elimina
(defrule RULES::remove-if-condition-when-satisfied
    ?f <- (rule
        (certainty ?c1)
        (if ?attribute is ?valueA $?rest)
    )
    (travel-banchmark
        (name ?attribute)
        (value ?valueB)
        (certainty ?c2)
    )
    (test (or (eq ?valueA ?valueB) (and (eq ?valueA number) (integerp ?valueB)))); la condizione "if" va rimossa o se valueA=valueB oppure se l'attributo valueA è un numero e anche valueB lo è
    =>
    (modify ?f (certainty (min ?c1 ?c2)) (if ?rest)); se ?rest è vuoto rimane solo "if"
)

;Crea un nuovo banchmark basato sulle regole empiriche definite
(defrule RULES::perform-rule-consequent
    ?f <- (rule
        (certainty ?c1)
        (if) ;non ci sono "if" quindi tutto l'if è stato soddisfatto
        (then ?attribute is ?value with certainty ?c2 $?rest)
    )
    =>
    (modify ?f (then ?rest))
    (assert
        (travel-banchmark
            (name ?attribute)
            (value ?value)
            (certainty (* ?c1 ?c2));il prodotto di due probabilità mi indica la probabilità che si verifichino entrambe
        )
    )
)

;;;** FATTI MODULO RULES ***********************************************************************

(deffacts RULES::expert-rules
    (rule
        (if min-resort-star is unknown)
        (then min-resort-star is 3 with certainty 1.0)
    )
    (rule
        (if number-of-place is unknown)
        (then number-of-place is 3 with certainty 1.0)
    )
    (rule
        (if trip-type is culturale)
        (then tourism-type is culturale with certainty 1.0 and 
            tourism-type is religioso with certainty 0.5 and 
            tourism-type is enogastronomico with certainty 0.4 and
            tourism-type is balneare with certainty -0.2)
    )
    (rule
        (if trip-type is rilassante)
        (then tourism-type is balneare with certainty 0.8 and 
            tourism-type is montano with certainty 0.8 and 
            tourism-type is sportivo with certainty 0.2 and
            tourism-type is termale with certainty 0.6 and
            tourism-type is lacustre with certainty 0.6)
    )
    (rule
        (if personal-trait is avventura)
        (then tourism-type is naturalistico with certainty 0.8 and 
            tourism-type is balneare with certainty -0.3 and 
            tourism-type is sportivo with certainty 0.2 and
            tourism-type is termale with certainty 0.6)
    )
    (rule
        (if personal-trait is comodità)
        (then tourism-type is balneare with certainty 0.8 and 
            tourism-type is montano with certainty 0.8 and 
            tourism-type is enogastronomico with certainty 0.6 and
            tourism-type is naturalistico with certainty -0.1)
    )
)














