(ns monte-carlo.core)

(defn in-circle? [pv r]
  "take a vector representing point and radius return true if the
  point is inside the circle"
  (< (->>
      pv
      (map #(* % %))
      (reduce +))
     (* r r)))
(defn rand-point [r]
  "return a random point from (0,0) inclusive to (r,r) exclusive"
  (repeatedly 2 #(rand r)))
(defn monte-carlo [n r]
  "take the number of random points and radius return an estimate to
pi"
  (*' 4 (/ n)
      (loop [i n count 0]
        (if (zero? i)
          count
          (recur (dec i)
                 (if (in-circle? (rand-point r) r)
                   (inc count)
                   count))))))
(defn -main []
  (let [constant-pi Math/PI
        computed-pi (monte-carlo 10000000 2) ;; this may take some time on lower end machines
        difference (Math/abs (- constant-pi computed-pi))
        error (* 100 (/ difference constant-pi))]
    (println "world's PI: " constant-pi
             ",our PI: "    (double computed-pi)
             ",error: " error)))
