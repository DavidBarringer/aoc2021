;; Put the given solutions for the examples here
(setq test-sol-a 5)
(setq test-sol-b 12)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (mapcar (lambda (c) (mapcar 'parse-integer (split "," c))) (split " -> " s))) (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (let* ((maxes (get-maxes parsed-input))
	 (results (make-array (list (+ 1 (CDR maxes)) (+ 1 (CAR maxes))) :initial-element 0)))
    (loop for ((x1 y1) (x2 y2)) in parsed-input
	  if (= x1 x2)
	    do (loop for y from (min y1 y2) to (max y1 y2)
		     do (incf (aref results y x1) 1))
	  else if (= y1 y2)
		 do (loop for x from (min x1 x2) to (max x1 x2)
			  do (incf (aref results y1 x) 1)))
    (loop for x from 0 below (CAR (array-dimensions results))
	  sum (loop for y from 0 below (CADR (array-dimensions results))
		    if (>= (aref results x y) 2) count y))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (let* ((maxes (get-maxes parsed-input))
	 (results (make-array (list (+ 1 (CDR maxes)) (+ 1 (CAR maxes))) :initial-element 0)))
    (loop for ((x1 y1) (x2 y2)) in parsed-input
	  if (= x1 x2)
	    do (loop for y from (min y1 y2) to (max y1 y2)
		     do (incf (aref results y x1) 1))
	  else if (= y1 y2)
		 do (loop for x from (min x1 x2) to (max x1 x2)
			  do (incf (aref results y1 x) 1))
	  else if (OR (AND (> x2 x1) (> y1 y2)) (AND (> x1 x2) (> y2 y1)))
	    do (loop for x from (min x1 x2) to (max x1 x2)
		     for y from (max y1 y2) downto (min y1 y2)
		     do (incf (aref results y x)))
	  else
	    do (loop for x from (min x1 x2) to (max x1 x2)
		     for y from (min y1 y2) to (max y1 y2)
		     do (incf (aref results y x))))
    (loop for x from 0 below (CAR (array-dimensions results))
	  sum (loop for y from 0 below (CADR (array-dimensions results))
		    if (>= (aref results x y) 2) count y))))

(defun get-maxes (input)
  (loop for ((x1 y1) (x2 y2)) in input
        maximize (max x1 x2) into x
	maximize (max y1 y2) into y
	finally (return (cons x y))))
