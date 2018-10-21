;;;; Monte carlo integration to approximate pi

(defun in-circle-p (x y)
  "Checks if a point is in a unit circle"
  (< (+ (* x x) (* y y)) 1))

(defun monte-carlo (samples)
  "Returns an approximation of pi"
  (loop repeat samples
    with count = 0 
    do
      (when (in-circle-p (random 1.0) (random 1.0))
            (incf count))
    finally (return (* (/ count samples) 4.0))))

(defvar pi-estimate (monte-carlo 5000000))
(format t "Estimate: ~D ~%" pi-estimate)
(format t "Error: ~D%" (* (/ (abs (- pi-estimate pi)) pi) 100))
