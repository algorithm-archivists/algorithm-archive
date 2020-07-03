;;;; Iterated Function System implementation

(defstruct (point (:constructor make-point (x y))) x y)

;; not a very good function some tail-end recursion would be kinda sexy here
(defun chaos-game (n shape-points)
  (loop
    with point = (make-point (random 1.0) (random 1.0))
    with result = (list)
    for i from 1 to n do
    (push point result)
    (let ((rand-point (aref shape-points (random (length shape-points)))))
      (setf point
        (make-point
          (* 0.5 (+ (point-x rand-point) (point-x point)))
          (* 0.5 (+ (point-y rand-point) (point-y point))))))
    finally (return result)))

(defparameter *shape-points*
  (map
    'vector
    (lambda (e) (apply #'make-point e))
    `((0 0) (0.5 ,(sqrt 0.75)) (1 0)))) ; this is basically magic

(with-open-file (out "out.dat" :direction :output :if-exists :supersede)
  (flet ((format-point (p)
           (format nil "~f~c~f" (point-x p) #\tab (point-y p)))) ; this is a bit ugly, but it works
    (format out "~{~a~%~}" (map 'list #'format-point (chaos-game 1000 *shape-points*)))))
