;; earthfail
(ns experiments.core)

;; get a vector with chars and frequencies

(defn tree-string [st]
  "take a string st and return the huffmantree with the frequency of
   each character included"
  ;; vector of [character frequency] pair
  ;; for every char in string, added it to hash-map
  ;; with value one if it doesn't exist or increment its value
  (def cf-vec (vec
               (reduce (fn [m c]
                         (assoc m c (inc (get m c 0))))
                       {}
                       st)))
  ;; make a sorted list with nodes with bigger frequencies first
  ;; take the last two which will help in dividing the tree
  ;; the first and last elements before and after
  ;; the smallest two in the tree shouldn't change
  (loop [tree (sort-by last > cf-vec)]
    (if (< (count tree) 2)
      (first tree) ; only of tree is one node or nil
      (let [sorted-tree (sort-by last > tree)
            mid (take-last 2 sorted-tree)
            set-mid (set mid)
            func (complement (partial contains? set-mid))
            firsty (take-while func tree)
            [middle lasty] (split-at 2
                                     (drop-while func tree))]
        (recur
         (concat
          firsty
          ;; make a list with the two element in one list and
          ;; the sum of their frequencies e.g
          ;; '(((node1 f1) (node2 f2)) f1+f2)
          (list (list middle (reduce #(+ %1 (last %2)) 0 middle)))
          lasty))))))

(defn remove-freq [tree]
  "remove the frequencies in the huffmantree tree"
  (cond
    (char? tree) tree                 ; check if this is a branch
    ;; if the tree is a node and frequency then ignore frequency
    (integer? (second tree)) (remove-freq (first tree)) ;remove the frequency
    ;; if the tree consists of two nodes then apply to both and combine
    :else (list (remove-freq (first tree))
                (remove-freq (second tree)))))

(defn hash-tree [tree]
  "make a hashmap with code for each letter as key and the letter as
  value"
  (cond
    (char? tree) {"" tree}
    :else
    (let [left-map (hash-tree (first tree))
          right-map (hash-tree (second tree))
          func #(apply hash-map         ; apply hash-map because
                                        ; interleave return a seq
                       (interleave
                        (map (partial str %2) (keys %1)) ;add 0 or 1
                                        ;to the start
                                        ;of the keys
                        (vals %1)))]
      ;; add "0" to the keys of left nodes and "1" to the right nodes
      (merge (func left-map "0") (func right-map "1")))))


(defn coder [s hash-coder]
  "take a string s and return a coded string"
  (apply str (map hash-coder s)))

(defn decoder [s hash-decoder]
  "takes a string s and a hash-map hash-decoder and decode s"
  ;; code keyword in hashmap is for storing codes untill they are
  ;; complete and can be decoded with the decoder
  (get (reduce (fn [m code]             ; reduce return {:message
                                        ; message,:code _}
                 (let [new-code (str (m :code) code)]
                   (if-let  [letter (get hash-decoder new-code)]
                     ;; if there is a letter then add it to :message
                     ;; and revert :code to empty
                     (assoc (update m :message #(str % letter))
                            :code "")
                     ;; if there is not a letter then just add the
                     ;; code letter to the :code
                     (update m :code #(str % code)))))
               {:message "",:code ""}
               s)
       :message))           ;extract :message  value
;; ----------------EXAMPLE----------------
(def st "(bibbity bobbity)")

(def hash-decoder (->>
                   st
                   tree-string
                   remove-freq
                   hash-tree))
(def hash-coder (clojure.set/map-invert hash-decoder))
(println "coding...")
(def code (coder st hash-coder))
(clojure.pprint/pprint code)

(println "\ndecoding...")
(clojure.pprint/pprint (decoder code hash-decoder))
