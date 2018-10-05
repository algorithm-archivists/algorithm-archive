#lang racket
(define (in_circle x y)
  (< (+ (sqr x) (sqr y)) 1)
 )

(define (monte_carlo n)
  (/ (local ((define (monte_carlo* n count)
                  (if (= n 0)
                      count
                      (monte_carlo* (sub1 n) 
                                    (if (in_circle (random) (random)) 
                                        (add1 count)
                                        count
                                    )
                      )
                  )
               )) (monte_carlo* n 0)
        ) n)
  )


; In this case, the Monte Carlo algorithm returns the ratio
; between the area of the unit circle and the area of the enclosing square.
; To calculate Pi using this ratio, it has to be multiplied by four.
; (This step is generally not required. It is only used to approximate Pi.)
(display (* 4 (monte_carlo 1000)))
