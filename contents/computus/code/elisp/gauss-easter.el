(require 'cl-lib)
(require 'subr-x)

(defun computus (year &optional servois)
  "Calculates the day of Easter for a given YEAR.
Also computes its SERVOIS number when non-nil."
  (let* ((a (mod year 19))                  ; year's position on the 19-year metonic cycle
         (k (floor year 100))               ; century index
         (p (floor (+ 13 (* 8 k)) 25))      ; shift of metonic cycle, add a day offset every 300 years
         (q (floor k 4))                    ; correction for non-observed leap days
         (m (mod (+ 15 (- p) k (- q)) 30))  ; correction to starting point of calculation each century
         (d (mod (+ (* 19 a) m) 30))        ; number of days from March 21st until the full moon
         (n (mod (+ 4 k (- q)) 7))          ; century-based offset in weekly calculation
         (b (mod year 4))                   ; correction for leap days
         (c (mod year 7))                   ; also a correction for leap days
                                            ; days from d to next Sunday
         (e (mod (+ (* 2 b) (* 4 c) (* 6 d) n) 7)))
    ; historical corrections for April 26th and 25th
    (when (or (and (= d 29) (= e 6))
              (and (= d 28) (= e 6) (> a 10)))
      (setq e -1))
    (cl-values
     ; determination of the correct month for Easter
     (if (> (+ 22 d e) 31)
         (format "April %s" (+ d e -9))
       (format "March %s" (+ 22 d e)))
     ; optionally return a value for the Servois table
     (if servois (mod (+ 21 d) 31)))))

(princ (format "%s"
               (string-join
                '("The following are the dates of the Pascal full moon (using Servois\n"
                  "notation) and the date of Easter for 2020-2030 AD:\n"
                  "Year    Servois number    Easter\n"))))
(dolist (year (number-sequence 2020 2030))
  (cl-multiple-value-bind (easter servois) (computus year t)
    (princ (format "%-8s%-18s%-s\n" year servois easter))))
