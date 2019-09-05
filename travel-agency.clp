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
    (focus QUESTIONS RULES TRIP);Indico l'ordine dello stack focus
;    (printout t "È un buon inizio")
)


;COMBINA INSIEME I CF dei travel-banchmark che hanno stesso nome e value
(defrule MAIN::combine-certainties
  (declare (salience 100) (auto-focus TRUE))
  ?rem1 <- (travel-banchmark (name ?rel) (value ?val) (certainty ?c1))
  ?rem2 <- (travel-banchmark (name ?rel) (value ?val) (certainty ?c2))
  (test (neq ?rem1 ?rem2));neq = not EQuals to
  =>
  (retract ?rem1);cancella rem1
  (modify ?rem2 (certainty (- (+ ?c1 ?c2) (* ?c1 ?c2))));(c1+c2)-(c1*c2)
)

(deffacts MAIN::test-fact
    (travel-banchmark (name tourism-type) (value balneare) (certainty 0.7))
    (travel-banchmark (name tourism-type) (value montano) (certainty 0.4))
    (travel-banchmark (name tourism-type) (value sportivo) (certainty 0.8))
    (travel-banchmark (name tourism-type) (value enogastronomico) (certainty 0.2))
    (travel-banchmark (name resort-star) (value 3) (certainty 0.7))
    (travel-banchmark (name travel-region) (value piemonte) (certainty 0.7))
    (travel-banchmark (name travel-region) (value toscana) (certainty 0.4))
    (travel-banchmark (name travel-region) (value liguria) (certainty 0.6))
    (travel-banchmark (name travel-region) (value puglia) (certainty 0.1))
)


;;****************
;;* MODULO TRIP *
;;****************
(defmodule TRIP (import MAIN ?ALL))

;Località turistiche presenti sul territorio italiano
(deftemplate TRIP::place
    (slot name (default ?NONE));?NONE indica che il campo name è obbligatorio
    (slot region)
    (slot ID)
    (multislot coordinates (cardinality 2 2))
)

(deftemplate TRIP::tourism-type
    (slot place-ID)
    (slot type)
    (slot score)
)

;Alberghi presenti sul territorio italiano
(deftemplate TRIP::resort
    (slot name (default ?NONE));?NONE indica che il campo name è obbligatorio
    (slot star);stelle dell'albergo
    (slot rooms-number);numero di camere disponibili nell'hotel
    (slot place-ID);nome della località turistica
)

;;;** REGOLE MODULO TRIP ***********************************************************************

;Regola che permette di definire il grado di approrpiatezza di una location rispetto ai banchmark inseriti dall'utente
(defrule TRIP::select-location
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
    )
    (travel-banchmark (name resort-star) (value ?resort-star) (certainty ?c1))
    (travel-banchmark (name tourism-type) (value ?tourism-type) (certainty ?c2))
    =>
    (assert
        (travel-banchmark
            (name location);creiamo un banchmark di tipo location
            (value ?resort-name)
            (certainty (min ?c1 ?c2 (/ ?score 5)));score/5 mi da il grado di certainty con cui il place appartiene ad un tourism-type
        )
    )
)


;;;** FATTI MODULO TRIP ***********************************************************************

(deffacts TRIP::place-list
    (place (name rapallo) (region liguria) (coordinates 3 4) (ID 1))
    (tourism-type (place-ID 1) (type balneare) (score 5))
    (tourism-type (place-ID 1) (type montano) (score 3))
    
    (place (name diano) (region liguria) (coordinates 3 4) (ID 2))
    (tourism-type (place-ID 2) (type balneare) (score 5))
    (tourism-type (place-ID 2) (type montano) (score 2))
    (tourism-type (place-ID 2) (type sportivo) (score 4))

    (place (name alassio) (region liguria) (coordinates 3 4) (ID 3))
    (tourism-type (place-ID 3) (type balneare) (score 5))
    (tourism-type (place-ID 3) (type montano) (score 2))
)

(deffacts TRIP::resort-list
    (resort (name resort-1a) (star 3) (rooms-number 10) (place-ID 1))
    (resort (name resort-1b) (star 2) (rooms-number 4) (place-ID 1))
    (resort (name resort-1c) (star 3) (rooms-number 6) (place-ID 1))
    (resort (name resort-1d) (star 3) (rooms-number 3) (place-ID 1))

    (resort (name resort-2a) (star 3) (rooms-number 8) (place-ID 2))
    (resort (name resort-2b) (star 2) (rooms-number 2) (place-ID 2))
    (resort (name resort-2c) (star 4) (rooms-number 5) (place-ID 2))
    (resort (name resort-2d) (star 3) (rooms-number 3) (place-ID 2))

    (resort (name resort-3a) (star 3) (rooms-number 3) (place-ID 3))
    (resort (name resort-3b) (star 2) (rooms-number 6) (place-ID 3))
    (resort (name resort-3c) (star 2) (rooms-number 2) (place-ID 3))
    (resort (name resort-3d) (star 3) (rooms-number 3) (place-ID 3))
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
    (slot already-asked (default FALSE))
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
        (valid-answers number unknown)
    )
    (question
        (travel-banchmark travel-budget)
        (the-question "Quanto sei disposto a spendere?")
        (valid-answers number unknown)
    )
    (question
        (travel-banchmark people-number)
        (the-question "Quante persone partecipano al viaggio?")
        (valid-answers number unknown)
    )
    ;altre domande: regione che si vuole visitare? (se la seleziona si fa in modo che non possano esserci rules che ne considerano altre) (magari lasciare la scelta solo tra 4-5 regioni)
    ;regione che non si vuole visitare? (certainty -1.0)
    ;vacanza monotematica? (precondizioni per evitare che possano poi esserci altre opzioni)
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
;    (travel-banchmark
;        (name ?attribute)
;        (value ?valueB)
;    )
;    (test (integerp ?value));il value dell'attributo è un intero
    =>
    (modify ?f (then ?rest))
;    (if (integerp ?valueB) then (bind ?op-value ?valueB) else (bind ?op-value 0))
    (assert
        (travel-banchmark
            (name ?attribute)
;            (value (+ ?value ?op-value))
            (value ?value)
            (certainty (* ?c1 ?c2));il prodotto di due probabilità mi indica la probabilità che si verifichino entrambe
        )
    )
)
;Regola che permette di creare nuovi banchmark basandosi sul valore che l'utente ha inserito da terminale
(defrule RULES::perform-rule-consequent-with-change
    ?f <- (rule
        (certainty ?c1)
        (if)
        (then ?attribute change ?value with certainty ?c2 $?rest)
    )
    (travel-banchmark
        (name ?attribute)
        (value ?user-value)
        (inferred FALSE)
    )
    =>
    (modify ?f (then ?rest))
    (assert
        (travel-banchmark
            (name ?attribute)
            (value (+ ?value ?user-value))
            (certainty (/ (* ?c1 ?c2) 1.0));il significato è: (c1*c2)/100
        )
    )
)

;;;** FATTI MODULO RULES ***********************************************************************

(deffacts RULES::expert-rules
    ; - Conoscenza empirica
    ;Se una persona non indica la durata del viaggio
    ;significa che ci sono maggiori possibilità che il viaggio sia breve
    (rule
        (if travel-duration is unknown)
        (then travel-duration is 7 with certainty 0.6 and
              travel-duration is 10 with certainty 0.3
        )
    )
    ; - Conoscenza empirica
    ;Se una persona non indica i componenti del viaggio
    ;il sistema cosidera ci siano buone possibilità che il viaggio sia per quattro persone
    (rule
        (if people-number is unknown)
        (then people-number is 4 with certainty 0.7 and
            people-number is 6 with certainty 0.1
        )
    )
    (rule
        (if travel-budget is number)
        (then travel-budget change -50 with certainty 0.4 and
            travel-budget change +100 with certainty 0.7
        )
    )
)














