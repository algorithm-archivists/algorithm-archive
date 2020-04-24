#lang racket

(define (bogo_sort l)
  (if (is_sorted? l)
      l
      (bogo_sort (shuffle l))
    )
 )

(define (is_sorted? l)
  (if (> (length l) 1)
      (if (> (first l) (second l))
             false
             (is_sorted? (rest l))
         )
      true
     )
 )

(define unsorted_list '(20 -3 50 1 -6 59))
(display "unsorted list: ")
(displayln unsorted_list)
(display "sorted list: ")
(displayln (bogo_sort unsorted_list))
