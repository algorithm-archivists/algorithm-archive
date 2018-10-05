(ns fft.core
  (:require [complex.core :as c]))
;; complex is a jar for complex numbers
;; https://github.com/alanforr/complex
;; add [complex "0.1.11"] to :dependencies in your project.clj
;; and run lein repl or lein deps in the terminal
(defn matrix-mult
  "take a matrix m and a vector v which length is number of columns
  ,return a vector of applying dot-product between v and each row of
  m. the returned vector's length is the number of rows of m"
  [m v]
  (mapv (comp (partial apply c/+)
              (partial map c/* v))
        m))
(defn dft
  "take a vector of real numbers and return a vector of frequency
  space"
  [vx]
  (let [len (count vx)]
     (matrix-mult
      (partition len
                 (for [n (range len)
                       k (range len)]
                   ;; expresion below is
                   ;; e^(n*k*2*pi*(1/len)*(-i))
                   (c/exp (c/* n k
                               2 Math/PI
                               (/ len)
                               (c/complex 0 -1)))))
      vx)))
(defn fft [vx]
  (let [len (count vx)]
    (if (= len 1)
      vx
      ;;else
      (let [;; take values of vx in the even indices
            even-indices (keep-indexed #(if (even? %1) %2) vx)
            ;; take values in the odd indices
            odd-indices (keep-indexed #(if (odd? %1) %2) vx)
            ;; recursion
            even-fft (fft even-indices)
            odd-fft (fft odd-indices)
            ;; make a sequence of e^(-2pi*i*k/N) where N is the length
            ;; vx and k range from 0 to N/2
            omegas-half (map
                         (comp c/exp
                               (partial c/*
                                        (/ len)
                                        2 Math/PI
                                        (c/complex 0 -1)))
                         (range 0 (quot len 2)))
            ;; take the negative of the first sequence because
            ;; e^(-2pi*i*(k+N/2)/N=-e^(-2pi*i*k/N) where k ranges from
            ;; 0 to N/2 
            omegas-2half (map c/- omegas-half)
            mult-add (partial map #(c/+ %3 (c/* %1 %2)))]
        (concat (mult-add omegas-half odd-fft even-fft)
                (mult-add omegas-2half odd-fft even-fft))))))
(defn -main [& args]
    (let [vx [0 1 2 3]
          len (count vx)
          ;; calculate the next power of 2 after len
          ;; the reason behind this is to fill them with zeros for fft
          next-len (->>
                    [len 2]
                    (map #(Math/log %))
                    (apply /)
                    Math/ceil
                    (Math/pow 2)
                    int)
          ;; add zeros at the end of vx
          complete-vx (into vx (repeat (- next-len len) 0))
          fft-cvx (fft complete-vx)
          dft-cvx (dft complete-vx)
          diffv (mapv c/- fft-cvx dft-cvx)]
    (println "vx:" vx)
    (println "complete-vx:" complete-vx)
    (println "result from fft:" (map c/stringify fft-cvx))
    (println "result from dft:" (map c/stringify dft-cvx))
    (println "difference: " (map c/stringify diffv))))
