;;;; Thomas algorithm implementation in Common Lisp

(defun thomas (diagonal-a diagonal-b diagonal-c last-column)
  "Returns the solutions to a tri-diagonal matrix non-destructively"
  (let ((a (copy-seq diagonal-a))
         (b (copy-seq diagonal-b))
         (c (copy-seq diagonal-c))
         (d (copy-seq last-column)))
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
      (decf (svref d i) (* (svref c i) (svref d (1+ i)))))
    d))

(defparameter diagonal-a #(0 2 3))
(defparameter diagonal-b #(1 3 6))
(defparameter diagonal-c #(4 5 0))
(defparameter last-column #(7 5 3))

(format t "~{~f ~}" (coerce (thomas diagonal-a diagonal-b diagonal-c last-column) 'list))
