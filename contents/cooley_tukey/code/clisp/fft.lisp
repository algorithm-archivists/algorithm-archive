
(defun coefficient (time-index freq-index dft-len)
  "Calculates a single twiddle factor for the Fourier Transform."
  (exp (- (/ (* #c(0 1) 2.0 pi time-index freq-index)
             dft-len))))

(defun dft (data)
  "Performs the Discrete Fourier Transform"
  (let ((dft-len (length data)))
    (loop for freq-index from 0 below dft-len collect
      (loop for time-index from 0 below dft-len sum
        (* (coefficient time-index freq-index dft-len) (elt data time-index))))))

(defun merge-sub-ffts (evens odds)
  "Combines the FFTs of the even and odd indices."
  (let* ((fft-length (+ (length evens) (length odds)))
         ;; Calculate coefficients for the odd indices.
         (twiddle-factors (loop for i from 0 below (length odds)
                             collect (coefficient 1.0 i fft-length)))
         ;; Multiply values with coefficients.
         (odd-terms (mapcar #'* odds twiddle-factors)))
    ;; Combine the two FFTs.
    (concatenate 'list 
                 (mapcar #'+ evens odd-terms)
                 (mapcar #'- evens odd-terms))))

(defun cooley-tukey-rec (data)
  "Performs the Fourier Transform using the recursive Cooley-Tukey method."
  (if (<= (length data) 1)
      data
      (loop
        for i from 0 below (length data)
        ;; Split even and odd indexed elements into two seperate lists.
        if (evenp i)
          collect (elt data i) into evens
        else
          collect (elt data i) into odds
        finally
          ;; Calculate the Fourier Transform for the two smaller lists and
          ;; combine them into the Fourier Transform of the full input.
          (return (merge-sub-ffts (cooley-tukey-rec evens)
                                  (cooley-tukey-rec odds))))))

(defun reverse-bits (value num-bits)
  "Reverses the bits of a value"
  (if (= num-bits 1)
      value
      ;; Split bits into two parts.
      (let* ((num-low-bits (floor (/ num-bits 2))) 
             (num-high-bits (- num-bits num-low-bits))
             (bit-mask (- (expt 2 num-low-bits) 1))
             (lower-half (logand value bit-mask))
             (upper-half (ash value (- num-low-bits))))
        ;; Reverse the bits of each part, then swap the results.
        (logior (ash (reverse-bits lower-half num-low-bits) num-high-bits)
                (reverse-bits upper-half num-high-bits)))))

(defun bit-shuffle-indices (data)
  "Rearanges the elements in a list according to their bit-reversed indices."
  (loop 
    with num-bits = (floor (log (length data) 2)) 
    for i from 0 below (length data)
    collect (elt data (reverse-bits i num-bits))))

(defun butterfly (a b coeff)
  "Calculates a single butterfly."
  (values (+ a (* coeff b)) (- a (* coeff b))))

(defun butterfly-group (data start stride)
  "Calculates a single group of butterflies."
  (dotimes (i stride)
    ;; Take two elements which are stride apart and perform a butterfly on them.
    (let* ((first-elt-index (+ start i))
           (second-elt-index (+ start i stride))
           (first-elt (elt data first-elt-index))
           (second-elt (elt data second-elt-index))
           (coeff (coefficient 1.0 i (* 2 stride))))
    (multiple-value-bind (sum difference) (butterfly first-elt second-elt coeff)
      ;; Write results back into the list.
      (setf (elt data first-elt-index) sum)
      (setf (elt data second-elt-index) difference)))))

(defun cooley-tukey-iter (data)
  "Performs the Fourier Transform using the iterative Cooley-Tukey method."
  (loop
    ;; Bit-shuffle indices.
    with shuffled-data = (bit-shuffle-indices data)
    for stride = 1 then (* 2 stride)
    while (< stride (length shuffled-data))
    do
      ;; Compute butterfly groups for the current stride.
      (loop for i from 0 below (length shuffled-data) by (* 2 stride) do
        (butterfly-group shuffled-data i stride))
    finally (return shuffled-data)))

(defun approx-eql (list1 list2)
  (let ((diffs (mapcar #'(lambda (e1 e2) (abs (- e1 e2)))
                       list1
                       list2)))
    (loop for d in diffs always (< d 1e-9))))

(defun test-fft (data)
  (let ((dft-result (dft data))
        (rec-result (cooley-tukey-rec data))
        (iter-result (cooley-tukey-iter data)))
    (format T "~&DFT and recursive Cooley-Tukey approx. equal: ~a" 
              (approx-eql dft-result rec-result))
    (format T "~&DFT and iterative Cooley-Tukey approx. equal: ~a"
              (approx-eql dft-result iter-result))
    (format T "~&Recursive Cooley-Tukey and iterative Cooley-Tukey approx. equal: ~a" 
              (approx-eql rec-result iter-result))))

(test-fft '(0.0 0.25 0.5 0.75 0.0 -0.25 -0.5 -0.75))
