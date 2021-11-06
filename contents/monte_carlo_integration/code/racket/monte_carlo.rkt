#lang racket
(define (in_circle x y)
  (< (+ (sqr x) (sqr y)) 1)
 )

(define (monte_carlo_pi n)
  (* (/ (local ((define (monte_carlo* n count)
                  (if (= n 0)
                      count
                      (monte_carlo_pi* (sub1 n) 
                                    (if (in_circle (random) (random)) 
                                        (add1 count)
                                        count
                                    )
                      )
                  )
               )) (monte_carlo_pi* n 0)
        ) n) 4)
  )


(display  (monte_carlo_pi 1000))
