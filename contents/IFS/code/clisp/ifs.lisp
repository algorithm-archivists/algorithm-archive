;;;; Iterated Function System implementation

(defstruct (point (:constructor make-point (x y))) x y)

(defun chaos-game (iterations shape-points)
  (loop
    repeat iterations
    for rand-point = (svref shape-points (random (length shape-points)))
    for point = (make-point (random 1.0) (random 1.0)) ; starting point
    then (make-point
           (* 0.5 (+ (point-x rand-point) (point-x point)))
           (* 0.5 (+ (point-y rand-point) (point-y point)))) ; every subsequent point
    collect point))

(defparameter *shape-points*
  (map
    'vector
    (lambda (e) (apply #'make-point e))
    ;; the backquote allows us to selectively evaluate (sqrt 0.75) with the comma
    `((0 0) (0.5 ,(sqrt 0.75)) (1 0))))

;; output the data to the "out.dat" file
(with-open-file (out "out.dat" :direction :output :if-exists :supersede)
  (flet ((format-point (p)
           (format nil "~f~c~f" (point-x p) #\tab (point-y p)))) ; this is a bit ugly, but it works
    (format out "~{~a~%~}" (map 'list #'format-point (chaos-game 10000 *shape-points*)))))
