;;;; Verlet integration implementation in Common Lisp

(defun verlet (pos acc dt)
  "Finds the time it takes for an object to hit the ground using verlet integration"
  (loop
    with prev-pos = pos
    with temp-pos = 0
    with time = 0
    while (> pos 0) do
      (incf time dt)
      (setf temp-pos pos)
      ;the starting speed is assumed to be zero
      (setf pos (+ (* pos 2) (- prev-pos) (* acc dt dt)))
      (setf prev-pos temp-pos)
    finally (return time)))

(defun stormer-verlet (pos acc dt)
  "Finds the time and velocity when an object hits the ground using the stormer-verlet method"
  (loop
    with prev-pos = pos
    with time = 0
    with vel = 0
    with temp-pos = 0
    while (> pos 0) do
      (incf time dt)
      (setf temp-pos pos)
      (setf pos (+ (* pos 2) (- prev-pos) (* acc dt dt)))
      (setf prev-pos temp-pos)
      (incf vel (* acc dt))
    finally (return (values time vel))))

(defun velocity-verlet (pos acc dt)
  (loop
    with prev-pos = pos
    with time = 0
    with vel = 0
    while (> pos 0) do
      (incf time dt)
      (incf pos (+ (* vel dt) (* 0.5 acc dt dt)))
      (incf vel (* acc dt))
    finally (return (values time vel))))

(format T "Time for Verlet integration is: ~d~%" (verlet 5 (- 10) 0.01))
(multiple-value-bind (time vel)
  (stormer-verlet 5 (- 10) 0.01)
  (format T
    "Time for Stormer Verlet integration is: ~d~%Velocity for Stormer Verlet integration is: ~d~%"
    time vel))
(multiple-value-bind (time vel)
  (velocity-verlet 5 (- 10) 0.01)
  (format T
    "Time for Stormer Verlet integration is: ~d~%Velocity for Stormer Verlet integration is: ~d~%"
    time vel))
