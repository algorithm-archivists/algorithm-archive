;;;;Bubble sort implementation

;;;Swaps to elements in a list (complexity: O(n))
(defun swap (lst low high)
    (let ((list-tail (nthcdr low lst)))
    (rotatef (car list-tail) (elt list-tail (- high low)))
    lst))

(defun bubble_sort (lst)
    (dotimes (m (- (length lst) 1) lst)                     ;loop
        (dotimes (n (- (length lst) (+ 1 m)) lst)           ;loop   
            (if (> (nth n lst) (nth (+ n 1) lst))           ;if
            (swap lst n (+ n 1))                            ;then
            lst))))                                         ;else

;;Use of an array insead of a list would be faster
;;The built-in sort is also quicker (sort (list 5 4 3 2 1))
(print (bubble_sort (list 5 4 3 2 1)))