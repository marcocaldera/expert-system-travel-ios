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

(deffunction MAIN::get-price-per-night (?star ?people-number)
    (bind ?price (+ 50 (* 25 (- ?star 1))))

    (bind ?required-room (+ (div ?people-number 2) (mod ?people-number 2)))


    (bind ?full-price (* ?price ?required-room))

    ?full-price
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
    (focus RULES LOCATION TRIP TRIP2);Indico l'ordine dello stack focus
    ; (focus QUESTIONS RULES LOCATION TRIP TRIP2);Indico l'ordine dello stack focus
)

(deffacts MAIN::test-fact
    (travel-banchmark (name travel-duration) (value 5) (certainty 1.0))
    (travel-banchmark (name travel-budget) (value 9000) (certainty 1.0))
    (travel-banchmark (name people-number) (value 3) (certainty 1.0))
    (travel-banchmark (name min-resort-star) (value 3) (certainty 1.0))
    (travel-banchmark (name number-of-place) (value 3) (certainty 1.0))
    (travel-banchmark (name trip-type) (value culturale) (certainty 1.0))
    (travel-banchmark (name personal-trait) (value avventura) (certainty 1.0))
    (travel-banchmark (name favourite-region) (value piemonte) (certainty 1.0))
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
    (test (> (* ?rooms-number 2) ?people-number));Escludo i resort che non hanno abbastanza stanze
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

    (place (name milano) (region piemonte) (coordinates 45.4667971 9.1904984) (ID 5))
    (tourism-type (place-ID 5) (type balneare) (score 1))
    (tourism-type (place-ID 5) (type montano) (score 4))
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
    (resort (name resort-3c) (star 3) (rooms-number 2) (place-ID 3))
    (resort (name resort-3d) (star 3) (rooms-number 3) (place-ID 3))

    (resort (name resort-4a) (star 3) (rooms-number 4) (place-ID 4))
    (resort (name resort-4b) (star 3) (rooms-number 7) (place-ID 4))
    (resort (name resort-4c) (star 2) (rooms-number 6) (place-ID 4))
    (resort (name resort-4d) (star 4) (rooms-number 3) (place-ID 4))

    (resort (name resort-5a) (star 3) (rooms-number 4) (place-ID 5))
    (resort (name resort-5b) (star 3) (rooms-number 7) (place-ID 5))
)

;;****************
;;* MODULO TRIP *
;;
;; Modulo per gestire i viaggi
;;****************
(defmodule TRIP (import MAIN ?ALL) (import LOCATION ?ALL) (export ?ALL))

; (deftemplate TRIP::trip
;     (multislot resort-sequence)
; )

; (deftemplate TRIP::link
;     (slot resort1)
;     (slot resort2)
;     (slot distance)
; )
(deftemplate TRIP::trip
   (multislot resort-sequence)
   (multislot place-sequence)
   (multislot certainties)
   (multislot days-distribution)
   (multislot price-per-night)
)

;qui ci potrebbe stare il controllo ?duration >= ?number-of-place 
(defrule TRIP::first-in-permutation
    ; (travel-banchmark (name number-of-place) (value ?k))
   ; (k-combination ~0)
   (travel-banchmark (name location) (value ?resort-name) (certainty ?c))
   (travel-banchmark (name number-of-place) (value ?number-of-place))
   (travel-banchmark (name people-number) (value ?people-number))

   (resort (name ?resort-name) (place-ID ?ID) (star ?star-number))
   (place (name ?city) (ID ?ID))
   =>
   ;Imposto che vada fatto almeno un giorno in ogni meta (in base al numero di mete richieste)  
   (bind ?array (create$))
   (loop-for-count ?number-of-place do (bind ?array (insert$ ?array 1 1)))

   (assert (trip 
    (resort-sequence ?resort-name)
    (place-sequence ?city)
    (certainties ?c)
    (days-distribution ?array)
    (price-per-night (get-price-per-night ?star-number ?people-number))
    ))
)

(defrule TRIP::next-in-permutation
    (travel-banchmark (name number-of-place) (value ?k))
    (travel-banchmark (name people-number) (value ?people-number))

    ;recupero insieme le informazioni su un resort (con il luogo in cui è situato - city- e le info sul numero di stelle)
    (travel-banchmark (name location) (value ?resort-name) (certainty ?c))
    (place (name ?city) (ID ?ID))
    (resort (name ?resort-name) (place-ID ?ID) (star ?star-number))

   ; (k-combination ?k)
   ?p <- (trip
    (resort-sequence $?resorts)
    (place-sequence $?cities)
    (certainties $?certainties)
    (days-distribution $?d)
    (price-per-night $?prices))

   (test (< (length$ ?cities) ?k));il numero di città da visitare deve essere inferiore al vincolo sul numero di mete
   (test (not (member$ ?city ?cities)));la città non deve essere già presente (non visito due volte la stessa città in un viaggio)
   =>
   (assert (trip 
    (resort-sequence ?resorts ?resort-name)
    (place-sequence ?cities ?city)
    (certainties ?certainties ?c)
    (days-distribution ?d)
    (price-per-night ?prices (get-price-per-night ?star-number ?people-number))
    ))
)

(defrule TRIP::cleanup
   (declare (salience -5))
   (travel-banchmark (name number-of-place) (value ?k))
   ?p <- (trip (resort-sequence $?cities))
   (test (< (length$ ?cities) ?k))
   =>
   (retract ?p)
)

(defmodule TRIP2 (import MAIN ?ALL) (import TRIP ?ALL) (export ?ALL))

(defrule TRIP2::distribute-days
    (travel-banchmark (name number-of-place) (value ?k))
    (travel-banchmark (name travel-duration) (value ?duration))
    (travel-banchmark (name travel-budget) (value ?budget))

    ?p <- (trip
    (resort-sequence $?resorts)
    (place-sequence $?cities)
    (certainties $?certainties)
    (days-distribution $?days-distribution)
    (price-per-night $?prices))

    (test (= (length$ ?cities) ?k))
    (test (< (+ 0 (expand$ ?days-distribution)) ?duration))

    =>

    (retract ?p)

    (loop-for-count (?cnt1 1 (length$ ?days-distribution)) do

        (bind ?new-prices (replace$ ?prices ?cnt1 ?cnt1 (* (nth$ ?cnt1 ?prices) 2)))
        (bind ?new-days-distribution (replace$ ?days-distribution ?cnt1 ?cnt1 (+ (nth$ ?cnt1 ?days-distribution) 1))) ;replace all'intero di ?day-distribution tra gli indici che vanno da ?cnt1 a ?cnt1 (un solo elemento) con il numero che c'era +1

        ;Se la nuova distribuzione dei giorni supera il budget non creo il nuovo trip
        (assert (trip 
            (resort-sequence ?resorts)
            (place-sequence ?cities)
            (certainties ?certainties)
            (days-distribution ?new-days-distribution)
            (price-per-night ?new-prices))
        )

    )

)

; cancello i trip che superano il budget
(defrule TRIP2::cleanup-budget
    ?p <- (trip 
        (price-per-night $?prices))

    (travel-banchmark (name travel-budget) (value ?budget))

    (test (> (+ 0 (expand$ ?prices)) ?budget));lo 0 serve per evitare errori nel caso in cui ?prices sia vuoto

    =>

    (retract ?p)
)

(defrule TRIP2::clean
    (travel-banchmark (name travel-duration) (value ?duration))

    ?p <- (trip
    (resort-sequence $?resorts)
    (place-sequence $?cities)
    (days-distribution $?days-distribution))

    ?p2 <- (trip
    (resort-sequence $?resorts)
    (place-sequence $?cities)
    (days-distribution $?days-distribution))

    (test (neq ?p ?p2))
    (test (= (+ 0 (expand$ ?days-distribution)) ?duration))
    =>

    (retract ?p)
)

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














