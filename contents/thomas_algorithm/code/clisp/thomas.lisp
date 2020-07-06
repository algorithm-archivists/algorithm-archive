;;;; Thomas algorithm implementation in Common Lisp

(defmacro divf (place divisor)
  "Divides the value at place by divisor"
  `(setf ,place (/ ,place ,divisor)))

(defun helper (v1 v2 v3 row)
  (- (svref v1 row) (* (svref v2 row) (svref v3 (1- row)))))

(defun thomas (diagonal-a diagonal-b diagonal-c last-column)
  "Returns the solutions to a tri-diagonal matrix non-destructively"
  ;; We have to copy the inputs to ensure non-destructiveness
  (let ((a (copy-seq diagonal-a))
         (b (copy-seq diagonal-b))
         (c (copy-seq diagonal-c))
         (d (copy-seq last-column)))
    (divf (svref c 0) (svref b 0))
    (divf (svref d 0) (svref b 0))
    (loop
      for i from 1 upto (1- (length a)) do
      (divf (svref c i) (helper b a c i))
      (setf (svref d i) (/ (helper d a d i) (helper b a c i))))
    (loop
      for i from (- (length a) 2) downto 0 do
      (decf (svref d i) (* (svref c i) (svref d (1+ i)))))
    d))

(defparameter diagonal-a #(0 2 3))
(defparameter diagonal-b #(1 3 6))
(defparameter diagonal-c #(4 5 0))
(defparameter last-column #(7 5 3))

;; should print 0.8666667 1.5333333 -0.26666668
(format t "~{~f ~}~%" (coerce (thomas diagonal-a diagonal-b diagonal-c last-column) 'list))
