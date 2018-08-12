;;;; Bubble sort implementation

;;; Swaps two elements in a list (complexity: O(n))
(defun swap (lst low high)
  (let ((list-tail (nthcdr low lst)))
  (rotatef (car list-tail) (elt list-tail (- high low)))
  lst))

(defun bubble-sort (lst)
  (dotimes (m (- (length lst) 1) lst)           ;loop
    (dotimes (n (- (length lst) (+ 1 m)) lst)   ;loop   
      (if (> (nth n lst) (nth (+ n 1) lst))     ;if
      (swap lst n (+ n 1))                      ;then
      lst))))                                   ;else

;; The built-in sort: (sort (list 5 4 3 2 1) #'<)
(print 
  (bubble-sort (list 5 4 3 2 1)))
(print
  (bubble-sort (list 1 2 3 3 2 1)))
