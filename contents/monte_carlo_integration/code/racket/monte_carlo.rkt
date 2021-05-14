#lang racket/base

(require racket/local)
(require racket/math)

(define (in-circle x y)
  "Checks if a point is in a unit circle"
  (< (+ (sqr x) (sqr y)) 1))

(define (monte-carlo-pi n)
  "Returns an approximation of pi"
  (* (/ (local ((define (monte-carlo-pi* n count)
                  (if (= n 0)
                      count
                      (monte-carlo-pi* (sub1 n) 
                                       (if (in-circle (random) (random)) 
                                           (add1 count)
                                           count)))))
          (monte-carlo-pi* n 0)) n) 4))

(define nsamples 5000000)
(define pi-estimate (monte-carlo-pi nsamples))
(displayln (string-append "Estimate (rational): " (number->string pi-estimate)))
(displayln (string-append "Estimate (float): " (number->string (real->single-flonum pi-estimate))))
(displayln (string-append "Error:" (number->string (* (/ (abs (- pi-estimate pi)) pi) 100))))
