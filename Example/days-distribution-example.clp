(deftemplate trip
   (multislot place-sequence)
   (multislot days-distribution)
   (multislot price-per-night)
)
(deftemplate travel-banchmark
    (slot name)
    (slot value)
)

(deffacts initial
   (travel-banchmark (name travel-duration) (value 15))
   (travel-banchmark (name travel-budget) (value 2400))
   (travel-banchmark (name number-of-place) (value 3))
   (trip (place-sequence milano roma venezia) (price-per-night 100 100 100))
   (trip (place-sequence milano zio venezia)(price-per-night 100 100 100))
   (trip (place-sequence a zio venezia)(price-per-night 100 100 100))
   (trip (place-sequence milano zio b) (price-per-night 100 100 100))
   (trip (place-sequence c zio venezia) (price-per-night 100 100 100))
   (trip (place-sequence milano d venezia)(price-per-night 100 100 100))
)

; (defrule test
;     (travel-banchmark (name travel-duration) (value ?duration))

;     ?p <- (trip
;     (place-sequence $?cities)
;     (days-distribution $?days-distribution))

;     (test (< (+ 0 (expand$ ?days-distribution)) ?duration))

;     =>

;     (retract ?p)

;     (loop-for-count (?cnt1 1 (length$ ?days-distribution)) do
;         (bind ?new-days-distribution (replace$ ?days-distribution ?cnt1 ?cnt1 (+ (nth$ ?cnt1 ?days-distribution) 1)))

;         (assert (trip 
;             (place-sequence ?cities)
;             (days-distribution ?new-days-distribution))
;         )

;     )

; )

(defrule test
    (travel-banchmark (name travel-duration) (value ?duration))
    (travel-banchmark (name travel-budget) (value ?budget))
    ?p <- (trip (place-sequence $?cities) (price-per-night $?price-per-night) (days-distribution $?days-distribution))

    (travel-banchmark (name number-of-place) (value ?k&~unknown))
    (test (eq (length$ ?cities) ?k))
    (test (eq (length$ ?days-distribution) 0));Non devono essere già stata calcolata la distribuzione dei giorni
    =>
    (bind ?city-count (length$ ?cities)); Numero di città

    (bind ?days-distribution (create$))
    (loop-for-count (?cnt1 1 ?city-count)
        ; Distribuisco i giorni uniformemente
        (bind ?days-distribution (insert$ ?days-distribution 1 (div ?duration ?city-count)))
        ; Aggiorno i prezzi per notte in base ai giorni appena distribuiti
        (bind ?price-per-night (replace$ ?price-per-night ?cnt1 ?cnt1 (* (nth$ ?cnt1 ?price-per-night) (nth$ ?cnt1 ?days-distribution))))
    )

    (loop-for-count (?cnt1 1 ?city-count)
        ; Aggiungo i giorni rimanenti in posizione ?cnt1
        (bind ?new-days-distribution (replace$ ?days-distribution ?cnt1 ?cnt1 (+ (div ?duration ?city-count) (mod ?duration ?city-count))))
        (printout t ?new-days-distribution crlf)
        ; Aggiorno il costo delle notti nel place n° ?cnt1 considerano anche i nuovi giorni appena aggiuni
        (bind ?new-price-per-night (replace$ ?price-per-night ?cnt1 ?cnt1 (* (div (nth$ ?cnt1 ?price-per-night) (div ?duration ?city-count)) (nth$ ?cnt1 ?new-days-distribution))))
        (printout t ?new-price-per-night crlf)

        ; Se complessivamente non supera il budget, creo il trip
        (if (<= (+ 0 (expand$ ?new-price-per-night)) ?budget) then
        ;duplico il fatto ?p andando a modificare price-per-night e days-distribution
        (duplicate ?p (price-per-night ?new-price-per-night) (days-distribution ?new-days-distribution))
        )
    )

    (retract ?p))

;prodotto cartesiano, ci mette un'infinità
; (defrule clean
;     (declare (salience -5))

;     ?p <- (trip
;     (place-sequence $?cities)
;     (days-distribution $?days-distribution))

;     ?p2 <- (trip
;     (place-sequence $?cities)
;     (days-distribution $?days-distribution))

;     (test (neq ?p ?p2))
;     =>

;     (retract ?p)

; )



; PER TESTARE
; (length$ (find-all-facts ((?f trip)) TRUE))

;questa versione è quella lenta













