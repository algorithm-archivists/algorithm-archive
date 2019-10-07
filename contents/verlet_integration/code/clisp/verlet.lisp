;;;; Verlet integration implementation in Common Lisp

(defun verlet (pos acc dt)
  "Integrates Newton's equation for motion while pos > 0 using Verlet integration."
  (loop
    with prev-pos = pos
    for time = 0 then (incf time dt)
    while (> pos 0)
    ;; The starting speed is assumed to be zero.
    do (psetf
         pos (+ (* pos 2) (- prev-pos) (* acc dt dt))
         prev-pos pos)
    finally (return time)))

(defun stormer-verlet (pos acc dt)
  "Integrates Newton's equation for motion while pos > 0 using the Stormer-Verlet method."
  (loop
    with prev-pos = pos
    for time = 0 then (incf time dt)
    for vel = 0 then (incf vel (* acc dt))
    while (> pos 0)
    ;; Variables are changed simultaneously by 'psetf', so there's no need for a temporary variable.
    do (psetf
         pos (+ (* pos 2) (- prev-pos) (* acc dt dt))
         prev-pos pos)
    finally (return (list time vel))))

(defun velocity-verlet (pos acc dt)
  "Integrates Newton's equation for motion while pos > 0 using the velocity in calculations."
  (loop
    for time = 0 then (incf time dt)
    for vel = 0 then (incf vel (* acc dt))
    for p = pos then (incf p (+ (* vel dt) (* 0.5 acc dt dt)))
    while (> p 0)
    finally (return (list time vel))))

(format T "Time for Verlet integration: ~d~%" (verlet 5 -10 0.01))

(defvar stormer-verlet-result (stormer-verlet 5 -10 0.01))
(format T "Time for Stormer Verlet integration is: ~d~%" (first stormer-verlet-result))
(format T "Velocity for Stormer Verlet integration is: ~d~%" (second stormer-verlet-result))

(defvar velocity-verlet-result (velocity-verlet 5 -10 0.01))
(format T "Time for velocity Verlet integration is: ~d~%" (first velocity-verlet-result))
(format T "Velocity for velocity Verlet integration is: ~d~%" (second velocity-verlet-result))
