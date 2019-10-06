;;;; Euclidean algorithm implementation in Common Lisp

(defun euclid-sub (a b)
  "Finds the greatest common divsor for any two integers"
  (defun euclid-sub* (a b)
    "Finds the greatest common divisor for any two positive integers"
    (if (eql a b)
        a
        (if (> a b)
            (euclid-sub* (- a b) b)
            (euclid-sub* a (- b a)))))
  (euclid-sub* (abs a) (abs b)))

(defun euclid-mod (a b)
  "Finds the greatest common divisor for any two integers"
  (if (zerop b)
      (abs a)
      (euclid-mod b (mod a b))))

(print (euclid-sub (* 64 67) (* 64 81)))
(print (euclid-mod (* 128 12) (* 128 77)))

;; Quick test
(assert
  (eql (euclid-sub (* 64 67) (* 64 81))
       (gcd (* 64 67) (* 64 81))))

(assert
  (eql (euclid-mod (* 64 67) (* 64 81))
       (gcd (* 64 67) (* 64 81))))
