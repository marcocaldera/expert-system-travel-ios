(deftemplate travel-banchmark
   (slot name)
   (slot value))

(deftemplate permutation
   (multislot values))

(deffacts initial
   (number-of-place 3)
   (travel-banchmark (name place) (value torino))
   (travel-banchmark (name place) (value roma))
   (travel-banchmark (name place) (value milano))
   (travel-banchmark (name place) (value venezia))
   (travel-banchmark (name place) (value napoli)))

(defrule first-in-permutation
   (number-of-place ~0)
   (travel-banchmark (name place) (value ?city))
   =>
   (assert (permutation (values ?city))))

(defrule next-in-permutation
   (number-of-place ?k)
   ?p <- (permutation (values $?cities))
   (test (< (length$ ?cities) ?k))
   (travel-banchmark (name place) (value ?city))
   (test (not (member$ ?city ?cities)))
   =>
   (assert (permutation (values ?cities ?city))))
   
(defrule cleanup
   (declare (salience -5))
   (number-of-place ?k)
   ?p <- (permutation (values $?cities))
   (test (< (length$ ?cities) ?k))
   =>
   (retract ?p))

; PER TESTARE
; (length$ (find-all-facts ((?f permutation)) TRUE))