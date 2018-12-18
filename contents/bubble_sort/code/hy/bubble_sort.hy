(require [hy.contrib.walk [let]])

(defn cons [a b]
  (do
    (setv c (.copy (list b)))
    (.insert c 0 a)
    c))

(defn bubble-up [l]
  (if (< (len (list l)) 2)
      l
      (if (> (first l) (second l))
          (cons (second l)
                (bubble-up (cons (first l) (rest (rest l)))))
          (cons (first l)
                (bubble-up (list (rest l)))))))

(defn bubble-sort [l]
  (if (< (len (list l)) 2)
      l
      (let [new-list (bubble-up l)]
           (+ (bubble-sort (list (butlast new-list)))
              [(last new-list)]))))

(print (bubble-sort [1 45 756 4569 56 3 8 5 -10 -4]))
