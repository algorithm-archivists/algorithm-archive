
(defun coefficient (time-index freq-index dft-len)
  "Calculates a single twiddle factor for the Fourier Transform."
  (exp (- (/ (* #c( 0 1) 2.0 pi time-index freq-index)
             dft-len))))

(defun dft (data)
  "Performs the Discrete Fourier Transform"
  (let ((dft-len (length data)))
    (loop for freq-index from 0 below dft-len collect
      (loop for time-index from 0 below dft-len sum
        (* (coefficient time-index freq-index dft-len) (elt data time-index))))))

(defun merge-sub-ffts (evens odds)
  "Combines the FFTs of the even and odd indices"
  (let* ((fft-length (+ (length evens) (length odds)))
         (indices (loop for i from 0 below (length odds) collect i))
         (odd-terms (mapcar #'(lambda (v i) (* v (coefficient 1.0 i fft-length)))
                            odds
                            indices)))
    (concatenate 'list 
                 (mapcar #'+ evens odd-terms)
                 (mapcar #'- evens odd-terms))))

(defun cooley-tukey-rec (data)
  "Performs the DFT using the recursive Cooley-Tukey method."
  (if (<= (length data) 1)
    data
    (loop for i from 0 below (length data)
      if (evenp i)
        collect (elt data i) into evens
      else
        collect (elt data i) into odds
      finally
        (return (merge-sub-ffts (cooley-tukey-rec evens)
                                (cooley-tukey-rec odds))))))

(defun reverse-bits (value num-bits)
  "Reverses the bits of a value"
  (if (= num-bits 1)
    value
    (let* ((num-low-bits (floor (/ num-bits 2)))
           (num-high-bits (- num-bits num-low-bits))
           (bit-mask (- (expt 2 num-low-bits) 1))
           (lower-half (logand value bit-mask))
           (upper-half (ash value (- num-low-bits))))
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
    (let* ((a-index (+ start i))
           (b-index (+ start i stride))
           (a (elt data a-index))
           (b (elt data b-index))
           (coeff (coefficient 1.0 i (* 2 stride))))
    (multiple-value-bind (sum difference) (butterfly a b coeff)
      (setf (elt data a-index) sum)
      (setf (elt data b-index) difference)))))

(defun cooley-tukey-tailrec (data stride)
  "Actual iterative implementation of the Cooley-Tukey method."
  (if (>= stride (length data))
    data
    (progn
      (loop for i from 0 below (length data) by (* 2 stride) do
        (butterfly-group data i stride))
      (cooley-tukey-tailrec data (* 2 stride)))))

(defun cooley-tukey-iter (data)
  "Performs the DFT using the iterative Cooley-Tukey method."
  (cooley-tukey-tailrec (bit-shuffle-indices data) 1))

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
