;;;; Bubble sort implementation

(defun bubble-up (list)
  (if
    (< (length list) 2) 
    list  
    (if 
      (> (first list) (second list))
      (cons 
        (second list) 
        (bubble-up 
          (cons 
            (first list) 
            (rest (rest list)))))
            
      (cons 
        (first list)
        (bubble-up
          (cons
            (second list)
            (rest (rest list))))))))

(defun bubble-sort (list)
  (if 
    (equal list (bubble-up list))
    list
    (bubble-sort (bubble-up list))))

;; The built-in sort: (sort (list 5 4 3 2 1) #'<)
(print 
  (bubble-sort (list 5 4 3 2 1)))
(print
  (bubble-sort (list 1 2 3 3 2 1)))
