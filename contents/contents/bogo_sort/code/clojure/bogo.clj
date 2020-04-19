;; earthfail
(defn is-sorted [col func]
  "return true of col is sorted in respect to func role
   like <,>,<=,>="
  (apply func col))

(defn bogo-sort [col func]
  "shuffle the collection untill it is sorted"
  (if (is-sorted col func)
    col
    (shuffle col)))
