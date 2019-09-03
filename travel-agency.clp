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
    (slot certainty (default 100.0))
)

;;;** REGOLE MODULO MAIN **

;Punto di partenza del sistema esperto
(defrule MAIN::start
    (declare (salience 10000)) ;Indico la salience massima possibile (10.000)
    =>
    (set-fact-duplication TRUE);Permette di avere fatti duplicati
    (focus QUESTIONS);Indico l'ordine dello stack focus
;    (printout t "È un buon inizio")
)


;;****************
;;* MODULO PLACE *
;;****************
(defmodule PLACE)

(deftemplate PLACE::place "località turistiche presenti sul territorio italiano"
    (slot name (default ?NONE));?NONE indica che il campo name è obbligatorio
    (slot region)
    (multislot coordinates (cardinality 2 2))
    (multislot type-of-turism)
)

(deffacts PLACE::place-list
    (place (name rapallo) (region liguria) (coordinates 3 4) (type-of-turism balneare 5 and montano 3))
)


;;****************
;;* MODULO RESORT *
;;****************
(defmodule RESORT)

(deftemplate RESORT::resort "alberghi presenti sul territorio italiano"
    (slot name (default ?NONE));?NONE indica che il campo name è obbligatorio
    (slot star);stelle dell'albergo
    (slot rooms-number);numero di camere disponibili nell'hotel
    (slot place);nome della località turistica
)

(deffacts RESORT::resort-list
    (resort (name miramare) (star 3) (rooms-number 10) (place rapallo))
)


;;****************
;;* USER QUESTIONS *
;;****************
(defmodule QUESTIONS (import MAIN ?ALL) (export ?ALL))

(deftemplate QUESTIONS::question "template delle domande da porre all'utente"
    (slot travel-banchmark (default ?NONE))
    (slot the-question (default ?NONE))
    (multislot valid-answers (default ?NONE))
    (slot already-asked (default FALSE))
    (multislot precursors));possono anche non essercene
)


;;;** REGOLE MODULO QUESTIONS **

(defrule QUESTIONS::ask-a-question
    ?f <- (question (already-asked FALSE) ;se non ho ancora fatto la domanda
        (precursors);se la domanda non ha precursori
        (the-question ?the-question)
        (travel-banchmark ?the-attribute)
        (valid-answers $?valid-answers))
    =>
    (modify ?f (already-asked TRUE)) ;indico che ho posto la domanda
    (assert (travel-banchmark (name ?the-attribute)
    (value (ask-question ?the-question ?valid-answers)))));creo un nuovo attribute con la risposta alla domanda (dopo averla chiesta con ask-question)

;;;** FATTI MODULO QUESTIONS **

(deffacts QUESTIONS::question-list
    (question
        (travel-banchmark travel-duration)
        (the-question "Quanto deve durare il viaggio?")
        (valid-answers number unknown)
    )
)
