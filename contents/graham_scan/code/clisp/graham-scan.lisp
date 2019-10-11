;;;; Graham scan implementation in Common Lisp

(defstruct (point (:constructor make-point (x y))) x y)

(defun counterclockwise-p (p1 p2 p3)
  "Determines if a turn between three points is counterclockwise"
  (>=
    (*
      (- (point-y p3) (point-y p1))
      (- (point-x p2) (point-x p1)))
    (*
      (- (point-y p2) (point-y p1))
      (- (point-x p3) (point-x p1)))))

(defun atan2 (y x)
  "Calculates the angle of a point in the euclidean plane in radians"
  (cond
    ((> x 0)                    (atan y x))
    ((and (< x 0) (>= y 0))     (+ (atan y x) pi))
    ((and (< x 0) (< y 0))      (- (atan y x) pi))
    ((and (eql x 0) (> y 0))    (/ pi 2))
    ((and (eql x 0) (< y 0))    (- (/ pi 2)))
    ;the -1 signifies an exception and is usefull later for sorting by the polar angle
    ((and (eql x 0) (eql y 0)) -1)))

(defun polar-angle (ref point)
  "Returns the polar angle from a point relative to a reference point"
  (atan2 (- (point-y point) (point-y ref)) (- (point-x point) (point-x ref))))

(defun lowest-point (gift)
  "Returns the lowest point of a gift"
  (reduce
    (lambda (p1 p2)
      (if (< (point-y p1) (point-y p2)) p1 p2))
    gift))

(defun graham-scan (gift)
  "Finds the convex hull of any distribution of points destructively"
  (loop
    with lowest = (lowest-point gift)
    with sorted = (sort gift #'< :key (lambda (p) (polar-angle lowest p)))
    with hull = (subseq sorted 0 3)

    for point in (subseq sorted 3 nil)
    do
      (loop
        until (counterclockwise-p (nth (- (length hull) 2) hull) (nth (1- (length hull)) hull) point)
        do (setf hull (butlast hull)))
      (setf hull (append hull (list point)))
    finally (return hull)))

(defvar gift
  (map
    'list
    (lambda (e) (apply #'make-point e))
    '((-5 2) (5 7) (-6 -12) (-14 -14) (9 9)
      (-1 -1) (-10 11) (-6 15) (-6 -8) (15 -9)
      (7 -7) (-2 -9) (6 -5) (0 14) (2 8))))

(print (graham-scan gift))
