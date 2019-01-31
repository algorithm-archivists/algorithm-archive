;;;; Gaussian elimination implementation is Common Lisp

(defun maximum (list)
  "Returns the maximum value in a list"
  (reduce #'max list))

(defun swap-rows (r1 r2 matrix)
  "A funtion that returns a matrix where row r1 and row r2 are swapped"
  (let ((temp (svref matrix r1)))
    (setf (svref matrix r1) (svref matrix r2))
    (setf (svref matrix r2) temp)
    matrix))

;While there is a built in way for handeling two-dimensional arrays, it only allows manipulation
;of single elements and not entire rows. Nested vectors are used for the matrix here.
(defvar test-matrix #(#(2 3 4 6) #(1 2 3 4) #(3 -4 0 10)))

(print (swap-rows 1 2 test-matrix))
(print (swap-2-electric-boogaloo 1 2 test-matrix))
