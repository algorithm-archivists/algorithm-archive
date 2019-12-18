;;;; Graham scan implementation in Common Lisp

(defstruct (point (:constructor make-point (x y))) x y)

(defun ccw (p1 p2 p3)
  "Determines if a turn between three points is counterclockwise"
  (-
    (*
      (- (point-y p2) (point-y p1))
      (- (point-x p3) (point-x p1)))
    (*
      (- (point-y p3) (point-y p1))
      (- (point-x p2) (point-x p1)))))

(defun atan2 (y x)
  "Calculates the angle of a point in the euclidean plane in radians"
  (cond
    ((> x 0)                    (atan y x))
    ((and (< x 0) (>= y 0))     (+ (atan y x) pi))
    ((and (< x 0) (< y 0))      (- (atan y x) pi))
    ((and (eql x 0) (> y 0))    (/ pi 2))
    ((and (eql x 0) (< y 0))    (- (/ pi 2)))
    ;; The -1 signifies an exception and is usefull later for sorting by the polar angle
    ((and (eql x 0) (eql y 0))  -1)))

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
  "Finds the convex hull of a distribution of points with a graham scan"
  ;; An empty list evaluates to false (nil) and a non-empty list evaluates to true (t).
  ;; We can therefore use 'gift' instead of '(> (length gift) 0)'.
  (if gift
      (labels ((wrap (sorted-points hull)
                 (if sorted-points
                   ;; This covers the case where the hull has one or more element.
                   ;; We aren't concerned about the hull being empty, because then the gift must
                   ;; also be empty and this function is never given an empty gift.
                     (if (rest hull)
                         (if (<= (ccw (first sorted-points) (first hull) (second hull)) 0)
                             (wrap sorted-points (rest hull))
                             (wrap (rest sorted-points) (cons (first sorted-points) hull)))
                         (wrap (rest sorted-points) (list (first sorted-points) (first hull))))
                     hull)))
        ;; Because 'sort' shuffles things around destructively, graham-scan is also destructive. But
        ;; since the order of the points is generally not important, this shouldn't cause a problem.
        (let* ((lowest (lowest-point gift))
                (sorted (sort gift #'< :key (lambda (p) (polar-angle lowest p)))))
          (wrap sorted (list lowest))))
      nil))

(defvar gift
  (map
    'list
    (lambda (e) (apply #'make-point e))
    '((-5 2) (5 7) (-6 -12) (-14 -14) (9 9)
      (-1 -1) (-10 11) (-6 15) (-6 -8) (15 -9)
      (7 -7) (-2 -9) (6 -5) (0 14) (2 8))))

;; This should print out the following:
;; (#S(POINT :X -10 :Y 11) #S(POINT :X -6 :Y 15) #S(POINT :X 0 :Y 14)
;; #S(POINT :X 9 :Y 9) #S(POINT :X 7 :Y -7) #S(POINT :X -6 :Y -12))
(print (graham-scan gift))
