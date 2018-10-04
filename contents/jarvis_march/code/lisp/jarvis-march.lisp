;;;; Jarvis march implementation

(defstruct point x y)

(defun p-eql (p1 p2)
  "Checks if two points are the same"
  (and (eql (point-x p1) (point-x p2))
    (eql (point-y p1) (point-y p2))))

(defun leftmost-point (gift)
  "Returns the leftmost point of the gift"
  (if (< (length gift) 2)
      (first gift)
      (if 
        (< 
          (point-x (first gift)) 
          (point-x (second gift)))
        (leftmost-point 
          (cons
            (first gift)
            (rest (rest gift))))
        (leftmost-point
          (rest gift)))))

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
      Nil)))

(defun angle (p1 p2 p3)
  "Returns the angle between three points"
  (if (or (p-eql p1 p2) (p-eql p2 p3) (p-eql p1 p3))
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

(defun leftmost-point (gift)
  "Returns the leftmost point of a list of points"
  (if (< (length gift) 2)
      (first gift)
      (if 
        (< 
          (point-x (first gift)) 
          (point-x (second gift)))
        (leftmost-point 
          (cons
            (first gift)
            (rest (rest gift))))
        (leftmost-point
          (rest gift)))))

(defun next-point-on-hull (p1 p2 gift)
  "finds the next point on the convex hull of a gift"
  (cond 
    ((null gift) Nil) 
    ((null (rest gift)) (first gift)) 
    (t 
      (if 
        (> 
          (angle p1 p2 (first gift)) 
          (angle p1 p2 (second gift))) 
        (next-point-on-hull p1 p2 
          (cons 
            (first gift) 
            (rest (rest gift)))) 
        (next-point-on-hull p1 p2 (rest gift)))))) 

(defun jarvis-march (gift)
"finds the convex hull of any random distribution of points"
  (setq hull (leftmost-point gift))
  (setq hull
    (cons
      (next-point-on-hull
        (make-point 
          :x (point-x hull)
          :y (- (point-y hull) 1))
        hull
        gift)
      hull)) 
  (setq hull (list (first hull) (rest hull)))
  (loop 
    (if (p-eql (first hull) (first (last hull)))
        (return-from jarvis-march (butlast hull))
        (setq hull
          (cons
            (next-point-on-hull
              (second hull)
              (first hull)
              gift)
            hull)))))

(defun make-points (list)
  "Takes a list of lists and returns a list of points"
  (map 
    'list
    (lambda (e) (make-point :x (first e) :y (second e)))
    list))

(defvar test-set 
  (make-points 
   '((0 0) (2 1) (0 2)
    (-1 0) (1 -1) (0 1)
    (-2 0) (-1 -2) (1 -3))))

(print (jarvis-march test-set))
