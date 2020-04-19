#lang racket

(provide bubbleSort)


(define bubbleSort
  (case-lambda [(l) (bubbleSort l (length l))]
               [(l n) (if (= n 1)
                          l
                          (bubbleSort (pass l 0 n) (- n 1)))]))

; a single pass, if this is the nth pass, then we know that the (n - 1) last elements are already sorted
(define (pass l counter n)
  (let ([x (first l)]
        [y (second l)]
        [r (drop l 2)])
    (cond [(= (- n counter) 2) (cons (min x y) (cons (max x y) r))]
          [(cons (min x y) (pass  (cons (max x y) r) (+ counter 1) n))])))


((lambda (x) (display (bubbleSort x))) (read))
