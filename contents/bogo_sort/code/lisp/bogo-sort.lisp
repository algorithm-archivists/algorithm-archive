;;;; Bogo sort implementation

(defun sortedp (list)
  "Checks if a list is sorted"
  (if (< (length list) 2)
      t
      (if (<= (first list) (second list))
          (sortedp (rest list))
          nil)))

(defun shuffle (list)
  "Returns a shuffled list using the Fisher-Yates method"
  (loop for i from (1- (length list)) downto 0
    do
      (rotatef 
        (nth i list)
        (nth (random (1+ i)) list))
    finally (return list)))

(defun bogo-sort (list)
  "Sorts a given list (eventually)"
  (if (sortedp list)
      list
      (bogo-sort (shuffle list))))

(print (bogo-sort (list 1 3 2 4)))
