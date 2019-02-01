;;;; Gaussian elimination implementation is Common Lisp

;not used yet
(defun maximum (list)
  "Returns the maximum value in a list"
  (reduce #'max list))

(defun swap-rows (matrix r1 r2)
  "A funtion that returns a matrix where row r1 and row r2 are swapped"
  (let ((temp (svref matrix r1)))
    (setf (svref matrix r1) (svref matrix r2))
    (setf (svref matrix r2) temp)
    matrix))

(defun maximum-row (matrix column start)
  ;terrible explanation
  "Returns the row number with the highest value in a column of a matrix starting at a certain row"
  (let ((maximum start))
    (loop for row from start upto (1- (length matrix)) do
      (when
        (>
          (svref (svref matrix row) column)
          (svref (svref matrix maximum) column))
        (setq maximum row)))
    maximum))

(defun gaussian-elimination (matrix)
  ;column for now is just 0
  ;same goes for the starting row
  (let ((max-row (maximum-row matrix 0 0)))
    (if
      (eql (svref (svref matrix max-row) 0) 0)
      ;matrix is singular, there is no solution
      Nil
      (swap-rows matrix 0 max-row))))

;While there is a built in way for handeling two-dimensional arrays, it only allows manipulation
;of single elements and not entire rows. Nested vectors are used for the matrix here.
(defvar test-matrix #(#(2 3 4 6) #(1 2 3 4) #(3 -4 0 10)))

(print test-matrix)
(print (maximum-row test-matrix 0 0))
(print (gaussian-elimination test-matrix))
(print (swap-rows test-matrix 1 2))
