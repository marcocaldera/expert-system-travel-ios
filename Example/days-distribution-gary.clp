
(deftemplate trip
   (multislot place-sequence)
   (multislot days-distribution))

(deftemplate travel-banchmark
    (slot name)
    (slot value))

(deffacts initial
   (travel-banchmark (name travel-duration) (value 13))
   (trip (place-sequence milano roma venezia) (days-distribution))
   (trip (place-sequence milano zio venezia) (days-distribution))
   (trip (place-sequence a zio venezia) (days-distribution))
   (trip (place-sequence milano zio b) (days-distribution))
   (trip (place-sequence c zio venezia) (days-distribution))
   (trip (place-sequence milano d venezia) (days-distribution))
)

(deffunction create-distributions (?cc ?cities ?days ?duration $?distribution)
   (bind ?max-alloc (- ?duration ?days (- ?cc 1)))
   ; (printout t ?duration crlf)
   ; (printout t ?days crlf)
   ; (printout t (- ?cc 1) crlf)   
   ; (printout t "Res:"?max-alloc crlf)
   ; (printout t "------" crlf)
   (if (= ?cc 1)
      then
      (assert (trip (place-sequence ?cities) (days-distribution ?distribution ?max-alloc)))
      (return))
   (loop-for-count (?a ?max-alloc)
    ; (printout t ?distribution crlf)
      (create-distributions (- ?cc 1) ?cities (+ ?days ?a) ?duration ?distribution ?a)))
 
(defrule test
    (travel-banchmark (name travel-duration) (value ?duration))
    ?p <- (trip (place-sequence $?cities) (days-distribution))
    =>
    (bind ?city-count (length$ ?cities))
    (create-distributions ?city-count ?cities 0 ?duration)
    (retract ?p))

; PER TESTARE
; (length$ (find-all-facts ((?f trip)) TRUE))