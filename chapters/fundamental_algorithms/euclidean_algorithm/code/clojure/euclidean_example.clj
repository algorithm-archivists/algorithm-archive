(defn euclid-sub [a b]
  (loop [a a b b]
	(if (= a b)
	    a
	  (if (> a b)
	      (recur (- a b) b)
	    (recur a (- b a))))))
(defn euclid-mod [a b]
  (loop [a a b b]
	(if (zero? b)
	    a
	  (recur b (% a b)))))
