#lang racket
(define (in_circle x y)
  (< (+ (sqr x) (sqr y)) 1)
 )

(define (monte_carlo n)
  (* (/ (local ((define (monte_carlo_internal n pi_count)
            (if (= n 0)
                pi_count
                (monte_carlo_internal (sub1 n) (if (in_circle (random) (random)) 
						(add1 pi_count) pi_count
					       )
			)
                )
            ))(monte_carlo_internal n 0)
    ) n) 4)
  )

(monte_carlo 100000)
