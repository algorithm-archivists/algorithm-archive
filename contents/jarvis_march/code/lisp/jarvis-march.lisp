;;;; Jarvis March implementation

(defstruct (point (:constructor make-point (x y))) x y)

(defun is-left-p (p1 p2 p3)
  "Checks if the point p3 is to the left of the line p1 -> p2"
  (>
    (*
      (- (point-y p3) (point-y p1))
      (- (point-x p2) (point-x p1)))
    (*
      (- (point-y p2) (point-y p1))
      (- (point-x p3) (point-x p1)))))

(defun next-point-on-hull (p1 p2 gift)
  "Finds the next point on the convex hull of a gift"
  (if (null gift)
      p2
      (if (is-left-p p1 p2 (first gift))
          (next-point-on-hull p1 (first gift) (rest gift))
          (next-point-on-hull p1 p2 (rest gift)))))

(defun leftmost-point (gift)
  "Returns the lefmost point of a gift"
  (reduce 
    (lambda (p1 p2)
      (if (< (point-x p1) (point-x p2)) p1 p2))
    gift))

(defun jarvis-march (gift)
  "finds the convex hull of any distribution of points"
  ;deals with the edge cases
  (if (< (length gift) 3)
    gift
    (loop
      with start = (leftmost-point gift)
      with hull = (list start (make-point (point-x start) (- (point-y start) 1)))
      do 
        (setq hull
          (cons 
            (next-point-on-hull (first hull) (second hull) gift)
            hull))
      until (equalp (first hull) start)
      ;deletes extra points
      finally (return (rest (butlast hull))))))

(defvar gift
  (map 
    'list
    (lambda (e) (apply #'make-point e))
    '((2 1.5) (1 1) (2 4) (3 1))))

(print (jarvis-march gift))
