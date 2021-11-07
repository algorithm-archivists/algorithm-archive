;;;; Forward euler implementation in Common Lisp

(defun solve-euler (timestep n)
  "Returns a function where y'(t) = -3t and y(0) = 0 using the forward euler method"
  (loop
    with result = (make-array n :initial-element 1)
    for i from 1 upto (1- n) do
    (setf (svref result i) (- (svref result (1- i)) (* 3 (svref result (1- i)) timestep)))
    finally (return result)))

(defun approximatep (result threshold timestep)
  "Checks the result from the solve-euler function"
  (loop
    with approximatep = t
    with solution = 0
    for i from 0 upto (1- (length result)) do
    (setf solution (exp (* (- 3) i timestep)))
    (when (> (- (svref result i) solution) threshold)
      (setf approximatep nil)
      (format t "~d ~d~%" (svref result i) solution))
    finally (return approximatep)))

(defvar timestep 0.01)
(defvar n 100) ; number of steps
(defvar threshold 0.01)

(defvar result (solve-euler timestep n))
(defvar approximatep (approximatep result threshold timestep))
(format t "~:[Value(s) not in threshold~;All values within threshold~]~%" approximatep)
