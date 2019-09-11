(deftemplate trip
   (multislot place-sequence)
   (multislot days-distribution)
)
(deftemplate travel-banchmark
    (slot name)
    (slot value)
)

(deffacts initial
   (travel-banchmark (name travel-duration) (value 9))
   (trip (place-sequence milano roma venezia) (days-distribution 1 1 1))
)

(defrule test
    (travel-banchmark (name travel-duration) (value ?duration))

    ?p <- (trip
    (place-sequence $?cities)
    (days-distribution $?days-distribution))

    (test (< (+ 0 (expand$ ?days-distribution)) ?duration))

    =>

    (retract ?p)

    (loop-for-count (?cnt1 1 (length$ ?days-distribution)) do
        (bind ?new-days-distribution (replace$ ?days-distribution ?cnt1 ?cnt1 (+ (nth$ ?cnt1 ?days-distribution) 1)))

        (assert (trip 
            (place-sequence ?cities)
            (days-distribution ?new-days-distribution))
        )

    )

)

(defrule clean
    (declare (salience -5))

    ?p <- (trip
    (place-sequence $?cities)
    (days-distribution $?days-distribution))

    ?p2 <- (trip
    (place-sequence $?cities)
    (days-distribution $?days-distribution))

    (test (neq ?p ?p2))
    =>

    (retract ?p)

)



; PER TESTARE
; (length$ (find-all-facts ((?f trip)) TRUE))













