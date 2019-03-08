;;;; Gaussian elimination implementation in Common Lisp

(defun swap-rows (matrix r1 r2)
  "Swaps two rows in a matrix destructively"
  (loop for i upto (1- (array-dimension matrix 1))
    do (rotatef (aref matrix r1 i) (aref matrix r2 i))))

(defun max-index (matrix pivot-row pivot-col)
  "Returns the index with the highest value in a column, starting at the pivot row"
  (loop
    with max-row = pivot-row
    for i from pivot-row upto (1- (array-dimension matrix 0))
    do (when (> (aref matrix i pivot-col) (aref matrix max-row pivot-col))
      (setf max-row i))
    finally (return max-row)))

(defun gaussian-elimination (matrix)
  "Turns any matrix into row echelon form destructively"
  (loop
    with pivot-row = 0
    for pivot-col upto (- (array-dimension matrix 0) 2) do
      (swap-rows matrix pivot-row (max-index matrix pivot-row pivot-col))
      (loop for i from (1+ pivot-row) upto (1- (array-dimension matrix 0))
        do
          (loop for j from (1+ pivot-col) upto (1- (array-dimension matrix 1))
            with fraction = (/ (aref matrix i pivot-col) (aref matrix pivot-row pivot-col)) do
              (setf
                (aref matrix i j)
                (- (aref matrix i j) (* (aref matrix pivot-row j) fraction))))
        (setf (aref matrix i pivot-col) 0))
      (incf pivot-row)))

(defun gauss-jordan-elimination (matrix)
  "Turns a row echelon matrix into a reduced row echelon matrix destructively"
  (loop
    with row = 0
    for col from 0 upto (- (array-dimension matrix 1) 2) do
      (when (not (eql (aref matrix row col) 0))
        (loop for i from (1- (array-dimension matrix 1)) downto col do
          (setf
            (aref matrix row i)
            (/ (aref matrix row i) (aref matrix row col))))
        (loop for i from 0 upto (1- row) do
          (loop for j from (1- (array-dimension matrix 1)) downto col do
            (setf
              (aref matrix i j)
              (- (aref matrix i j) (* (aref matrix i col) (aref matrix row j))))))
        (incf row))))

(defun back-substitution (matrix)
  "Takes in a row echelon matrix and returns the solution to the system"
  (loop
    with solution = (make-array (array-dimension matrix 0) :initial-element 0)
    with sum = 0
    for i from (1- (array-dimension matrix 0)) downto 0 do
      (loop for j from (1- (array-dimension matrix 0)) downto i do
        (setf sum (+ sum (* (aref solution j) (aref matrix i j)))))
      (setf
        (aref solution i)
        (/ (- (aref matrix i (1- (array-dimension matrix 1))) sum) (aref matrix i i)))
      (setf sum 0)
    finally (return solution)))

(defvar matrix (make-array '(3 4) :initial-contents '((2 3 4 6) (1 2 3 4) (3 -4 0 10))))
(gaussian-elimination matrix)
(print matrix)
(print (back-substitution matrix))
(gauss-jordan-elimination matrix)
(print matrix)
