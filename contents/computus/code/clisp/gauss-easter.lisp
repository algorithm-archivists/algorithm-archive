;;;; Gauss's Easter algorithm implementation

(defun computus (year &optional (servois nil))
  "Calculates the day of Easter for a given year and optionally its Servois number"
  (let*
    ((a (mod year 19))                  ; year's position on the 19 year metonic cycle
      (k (floor year 100))              ; century index
      (p (floor (+ 13 (* 8 k)) 25))     ; shift of metonic cycle, add a day offset every 300 years
      (q (floor k 4))                   ; correction for non-observed leap days
      (m (mod (+ 15 (- p) k (- q)) 30)) ; correction to starting point of calculation each century
      (d (mod (+ (* 19 a) m) 30))       ; number of days from March 21st until the full moon
      (n (mod (+ 4 k (- q)) 7))         ; century-based offset in weekly calculation
      (b (mod year 4))                  ; correction for leap days
      (c (mod year 7))                  ; also a correction for leap days
      ;; days from d to next Sunday
      (e (mod (+ (* 2 b) (* 4 c) (* 6 d) n) 7)))
    ;; historical corrections for April 26 and 25
    (when (or (and (eql d 29) (eql e 6)) (and (eql d 28) (eql e 6) (> a 10)))
      (setf e -1))
    (values
      ;; determination of the correct month for Easter
      (if (> (+ 22 d e) 31)
          (format nil "April ~a" (+ d e -9))
          (format nil "March ~a" (+ 22 d e)))
      ;; optionally return a value for the Servois' table
      (if servois (mod (+ 21 d) 31)))))

(format t "~{~a~%~}"
  '("The following are the dates of the Paschal full moon (using Servois"
     "notation) and the date of Easter for 2020-2030 AD:~%"
     "Year    Servois number    Easter"))
(loop for year from 2020 to 2030 do
  (multiple-value-bind (easter servois) (computus year t)
    (format t "~8a~18a~a~%" year servois easter)))
