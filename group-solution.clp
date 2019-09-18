(deftemplate trip
   (multislot certainties)
   (multislot place-sequence))

(deffacts initial
   (trip (certainties 0.7) (place-sequence torino roma milano))
   (trip (certainties 0.7) (place-sequence milano roma torino))
   (trip (certainties 0.4) (place-sequence torino roma milano))
   (trip (certainties 0.6) (place-sequence roma torino milano))

   (trip (certainties 0.9) (place-sequence venezia roma torino))
   (trip (certainties 0.4) (place-sequence venezia torino roma)))

(defrule all-lamps-are-on 
    ; ?p <- (trip (certainties ?certainty) (place-sequence $?place-sequence))
   ?trip <- (trip (certainties ?certainty) (place-sequence $?place-sequence1))
   (and
      (trip (place-sequence $?place-sequence2) (certainties ?certainty2&:(> ?certainty2 ?certainty)))
      (test (subsetp $?place-sequence1 $?place-sequence2))
   )
  =>
  (retract ?trip)
  (printout t ?place-sequence1 crlf)
  (printout t ?certainty crlf)
) 

; PER TESTARE
; (length$ (find-all-facts ((?f permutation)) TRUE))