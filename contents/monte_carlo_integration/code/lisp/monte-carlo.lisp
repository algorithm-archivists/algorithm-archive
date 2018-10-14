;;;; Monte carlo integration to approximate pi

(defun in-circle-p (x y)
  "Checks if a point is in a unit circle"
  (< (+ (* x x) (* y y)) 1))

(defun monte-carlo (samples)
  (loop repeat samples
    with count = 0 
    do
      (when (in-circle-p (random 1.0) (random 1.0))
            (incf count))
    finally (return (* (/ count samples) 4.0))))

(print (monte-carlo 1000000))
