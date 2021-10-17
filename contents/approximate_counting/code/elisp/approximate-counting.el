(require 'cl-extra)

(defun sum (things)
  "Return the sum of all THINGS."
  (apply '+ things))

(defun n (v a)
  "Return the approximate count at a given register value V.
A is a scaling value for the logarithm based on Morris' paper."
  (* a (- (expt (+ 1 (/ 1 (float a))) v) 1)))

(defun increment (v a)
  "Return V, randomly incremented by one or as-is.
A is a scaling value for the logarithm based on Morris' paper."
  (let ((delta (/ 1 (- (n (1+ v) a) (n v a)))))
    (if (<= (cl-random 1.0) delta)
        (1+ v)
      v)))

(defun approximate-count (nitems a)
  "Simulate the counting of NITEMS, returning an approximate count.
A is a scaling value for the logarithm based on Morris' paper."
  (let ((v 0))
    (dotimes (i nitems)
      (setq v (increment v a)))
    (n v a)))

(defun test-approximate-count (ntrials nitems a threshold)
  "Test approximate counting, printing when passing.
NITEMS items are counted for NTRIALS trials to within a THRESHOLD error
(float between 0 and 1).
A is a scaling value for the logarithm based on Morris' paper."
  (let* ((samples (mapcar (lambda (x) (approximate-count nitems a)) (number-sequence 1 ntrials)))
         (avg (/ (sum samples) ntrials)))
    (if (< (abs (/ (- avg nitems) nitems)) threshold)
        (princ "passed\n"))))

(princ "testing 1,000, a = 30, 1% error\n")
(test-approximate-count 100 1000 30 0.1)
(princ "testing 12,345, a = 10, 1% error\n")
(test-approximate-count 100 12345 10 0.1)
(princ "testing 222,222, a = 0.5, 10% error\n")
(test-approximate-count 100 222222 0.5 0.2)
