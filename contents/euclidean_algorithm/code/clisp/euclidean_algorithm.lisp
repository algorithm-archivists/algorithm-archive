;;;; Euclid algorithm implementation

(defun euclid-sub (a b)
  (euclid-sub* (abs a) (abs b)))

(defun euclid-sub* (a b)
  (if (eq a b)
    a
    (if (> a b)
      (euclid-sub* (- a b) b)
      (euclid-sub* a (- b a)))))

(defun euclid-mod (a b)
  (if (zerop b)
    (abs a)
    (euclid-mod b (mod a b))))

(print 
  (euclid-sub (* 64 67)
              (* 64 81)))
(print 
  (euclid-mod (* 128 12)
              (* 128 77)))

;; built-in function: (gcd 80 40)
;; quick test
(assert 
  (=  (euclid-sub (* 64 67) (* 64 81))
      (gcd (* 64 67) (* 64 81))))

(assert 
  (=  (euclid-mod (* 64 67) (* 64 81))
      (gcd (* 64 67) (* 64 81))))
