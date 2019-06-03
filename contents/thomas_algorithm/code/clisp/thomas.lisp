;;;; Thomas algorithm implementation in Common Lisp

(defun thomas (a b c d)
  "Returns the solutions to a tri-diagonal matrix destructively"
  (setf (svref c 0) (/ (svref c 0) (svref b 0)))
  (setf (svref d 0) (/ (svref d 0) (svref b 0)))

  (loop
    for i from 1 upto (1- (length a)) do
      (setf
        (svref c i)
        (/ (svref c i) (- (svref b i) (* (svref a i) (svref c (1- i))))))
      (setf
        (svref d i)
        (/
          (- (svref d i) (* (svref a i) (svref d (1- i))))
          (- (svref b i) (* (svref a i) (svref c (1- i)))))))

  (loop
    for i from (- (length a) 2) downto 0 do
      (decf (svref d i) (* (svref c i) (svref d (1+ i))))
    finally (return d)))

(defvar diagonal-a #(0 2 3))
(defvar diagonal-b #(1 3 6))
(defvar diagonal-c #(4 5 0))
(defvar last-column #(7 5 3))

(print (thomas diagonal-a diagonal-b diagonal-c last-column))
