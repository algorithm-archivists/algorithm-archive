#lang racket

(define (euclid_sub a b)
  (local ((define (euclid_sub* x y)
          (if (= x y)
              x
              (if (> x y)
                  (euclid_sub* (- x y) y)
                  (euclid_sub* x (- y x))
                  )
              )
          )) (euclid_sub* (abs a) (abs b))
    )
  )

(define (euclid_mod a b)
  (local ((define (euclid_mod* a b)
           (if (= 0 b)
               (abs a)
               (euclid_mod* b (modulo a b))
               )
           )) (euclid_mod* a b)
    )
  )

(displayln (euclid_sub (* 64 67) (* 64 81)))
(displayln (euclid_mod (* 128 12) (* 128 77)))
