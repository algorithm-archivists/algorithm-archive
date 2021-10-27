(require 'cl-seq)

(defun -concat (&rest lists)
  "Return a new list with the concatenation of the elements in the supplied LISTS."
  (declare (pure t) (side-effect-free t))
  (apply 'append lists))

(defun -snoc (list elem &rest elements)
  "Append ELEM to the end of the list.

This is like `cons', but operates on the end of list.

If ELEMENTS is non nil, append these to the list as well."
  (-concat list (list elem) elements))

(defun nthrev (n lst)
  "Return the Nth element of LST from the end."
  ;; (car (nthcdr n (reverse lst)))
  (nth (- (length lst) (1+ n)) lst))

(defun is-ccw (a b c)
  (>= (* (- (nth 1 c) (nth 1 a)) (- (nth 0 b) (nth 0 a)))
      (* (- (nth 1 b) (nth 1 a)) (- (nth 0 c) (nth 0 a)))))

(defun polar-angle (ref point)
  (atan (- (nth 1 point) (nth 1 ref)) (- (nth 0 point) (nth 0 ref))))

(require 'dash)

(defun graham-scan (initial-gift)
  (let* ((gift (cl-remove-duplicates initial-gift))
         ;; this is /only/ to get the starting point
         (min-sorted-gift (sort gift (lambda (p1 p2) (< (nth 1 p1) (nth 1 p2)))))
         (start (car min-sorted-gift))
         (trimmed-gift (cdr min-sorted-gift))
         (points (sort trimmed-gift (lambda (p1 p2) (< (polar-angle start p1)
                                                       (polar-angle start p2)))))
         (hull (list start (car points) (cadr points))))
    (dolist (point (cddr points))
      (while (not (is-ccw (nthrev 1 hull) (nthrev 0 hull) point))
        (setq hull (-remove-at (1- (length hull)) hull)))
      (setq hull (-snoc hull point)))
    hull))

(princ
 (graham-scan
  '((-5 2)
    (5 7)
    (-6 -12)
    (-14 -14)
    (9 9)
    (-1 -1)
    (-10 11)
    (-6 15)
    (-6 -8)
    (15 -9)
    (7 -7)
    (-2 -9)
    (6 -5)
    (0 14)
    (2 8))))
