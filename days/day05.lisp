;; Put the given solutions for the examples here
(setq test-sol-a 5)
(setq test-sol-b 12)

;; Split on -> then on , to get a list of co-ordinate pairs ((x1 y1) (x2 y2))
(defun parse-input (input-file)
  (mapcar (lambda (s) (mapcar (lambda (c) (mapcar 'parse-integer (split "," c))) (split " -> " s))) (get-file-lines input-file)))

;; Make an array of the grid with each value initially set to zero. Go through each co-ordinate pair
;; if either x1 & x2 or y1 & y2 are equal, go over the range of the other and increase the value
;; at that position by one. Finally go over the array and count the number of elements greater than 1
(defun part-a (parsed-input)
  (let* ((maxes (get-maxes parsed-input))
	 (results (make-array (list (+ 1 (CDR maxes)) (+ 1 (CAR maxes))) :initial-element 0)))
    (loop for ((x1 y1) (x2 y2)) in parsed-input
	  if (= x1 x2)
	    do (loop for y from (min y1 y2) to (max y1 y2)
		     do (incf (aref results y x1)))
	  else if (= y1 y2)
		 do (loop for x from (min x1 x2) to (max x1 x2)
			  do (incf (aref results y1 x))))
    (loop for i from 0 below (array-total-size results)
	  if (>= (row-major-aref results i) 2) count i)))

;; Has the same setup as part a, but includes cases for diagonals, there is a check if the gradient
;; is negative so that the range of y co-ords can decrease as x increases
(defun part-b (parsed-input)
  (let* ((maxes (get-maxes parsed-input))
	 (results (make-array (list (+ 1 (CDR maxes)) (+ 1 (CAR maxes))) :initial-element 0)))
    (loop for ((x1 y1) (x2 y2)) in parsed-input
	  if (= x1 x2)
	    do (loop for y from (min y1 y2) to (max y1 y2)
		     do (incf (aref results y x1)))
	  else if (= y1 y2)
		 do (loop for x from (min x1 x2) to (max x1 x2)
			  do (incf (aref results y1 x)))
	  else if (eq (> x2 x1) (> y1 y2))
	    do (loop for x from (min x1 x2) to (max x1 x2)
		     for y from (max y1 y2) downto (min y1 y2)
		     do (incf (aref results y x)))
	  else
	    do (loop for x from (min x1 x2) to (max x1 x2)
		     for y from (min y1 y2) to (max y1 y2)
		     do (incf (aref results y x))))
    (loop for i from 0 below (array-total-size results)
	  if (>= (row-major-aref results i) 2) count i)))

;; Gets the highest x co-ordinate and highest y co-ordinate and returns them as a dotted pair
(defun get-maxes (input)
  (loop for ((x1 y1) (x2 y2)) in input
        maximize (max x1 x2) into x
	maximize (max y1 y2) into y
	finally (return (cons x y))))
