(defun nquick-sort (sequence)
  "Apply quicksort to sequence. Changes the original sequence."
  (nquick-sort-range sequence 0 (1- (length sequence))))

(defun quick-sort (sequence)
  "Apply quicksort to sequence. Doesn't change the original sequence."
  (quick-sort-range sequence 0 (1- (length sequence))))

(defun nquick-sort-range (sequence low high)
  "Apply quicksort to the range within sequence bounded by low and high.
Modifies the original sequence."
  (when (< low high)
    (let ((part (%partition sequence low high)))
      (nquick-sort-range sequence low (1- part))
      (nquick-sort-range sequence (1+ part) high))
    sequence))

(defun quick-sort-range (sequence low high)
  "Apply quicksort to the range within sequence bounded by low and high.
Doesn't modify the original sequence."
  (let ((tmp (copy-seq sequence)))
    (nquick-sort-range tmp low high)
    tmp))

(defun %partition (sequence low high)
  (let ((pivot (elt sequence high))
	(i (1- low)))
    (loop :for j :from low :to (1- high) :do
	 (when (< (elt sequence j) pivot)
	   (incf i)
	   (rotatef (elt sequence i)
		    (elt sequence j))))
    (rotatef (elt sequence (1+ i))
	     (elt sequence high))
    (1+ i)))
