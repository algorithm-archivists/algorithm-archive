;;;; Jarvis march implementation

(defstruct (point (:constructor make-point (x y))) x y)

(defun extreme-of (func comp-op list)
  "Finds the value in a list that evaluates to the highest
  or lowest value when passed to a function"
  (if (< (length list) 2)
      (first list)
      ;the comp-op is either '>' or '<'
      (if
        (funcall comp-op 
          (funcall func (first list))
          (funcall func (second list)))
        (extreme-of func comp-op (cons (first list) (rest (rest list)))) 
        (extreme-of func comp-op (rest list)))))

(defun leftmost-point (gift)
  "Returns the leftmost point of the gift"
  (extreme-of #'point-x #'< gift))

(defun next-point-on-hull (p1 p2 gift)
  "finds the next point on the convex hull of a gift"
  (extreme-of (lambda (p3) (angle p1 p2 p3)) #'> gift))

(defun atan2 (y x)
  "Returns the angle based on the origin"
  (cond
    ((> x 0)
      (atan (/ y x)))
    ((> y 0)
      (- (/ pi 2) (atan (/ x y))))
    ((< y 0)
      (- (/ (- pi) 2) (atan (/ x y))))
    ((< x 0)
      (+ (atan (/ y x)) pi))
    ((and (zerop x) (zerop y))
      nil)))

(defun angle (p1 p2 p3)
  "Returns the angle between three points"
  (if (or (equalp p1 p2) (equalp p2 p3) (equalp p1 p3))
      0
      (let*
        ((thetaA 
            (atan2 
              (- (point-y p1) (point-y p2)) 
              (- (point-x p1) (point-x p2))))
          (thetaC 
            (atan2 
              (- (point-y p3) (point-y p2)) 
              (- (point-x p3) (point-x p2))))
          (theta (- thetaC thetaA)))
        (cond
          ((< theta 0) (+ theta (* 2 pi)))
          (t theta)))))


(defun jarvis-march (gift)
"finds the convex hull of any random distribution of points"
  ;deals with edge cases
  (if (< (length gift) 3)
      gift
    (let* 
      ((start 
        (leftmost-point gift))
      ;creates the hull with the first and second points of the hull in a list
      (hull 
        (list 
          (next-point-on-hull
            (make-point (point-x start) (- (point-y start) 1))
            start
            gift)
          start)))
      (loop 
        (if (p-eql (first hull) start)
            (return-from jarvis-march (butlast hull))
            (setq hull
              (cons
                (next-point-on-hull
                  (second hull)
                  (first hull)
                  gift)
                hull)))))))

(defvar test-gift
  (map 
    'list
    (lambda (e) (apply #'make-point e))
    '((0 0) (2 1) (0 2)
    (-1 0) (1 -1) (0 1)
    (-2 0) (-1 -2) (1 -3))))

(print (jarvis-march test-gift))
