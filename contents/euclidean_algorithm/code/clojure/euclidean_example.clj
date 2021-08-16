;; earthfail
(defn euclid-sub [a b]
  (loop [i (Math/abs a) j (Math/abs b)]
    (if (= i j)
      i
      (if (> i j)
        (recur (- i j) j)
        (recur i (- j i))))))
(defn euclid-mod [a b]
  (loop [i (Math/abs a) j (Math/abs b)]
    (if (zero? j)
      i
      (recur j (% i j)))))

(print
 (euclid-sub (* 64 67)
             (* 64 81))
 (euclid-mod (* 128 12)
             (* 128 77)))
