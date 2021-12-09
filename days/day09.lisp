;; Put the given solutions for the examples here
(setq test-sol-a 15)
(setq test-sol-b 1134)

;; Splits input on lines, then turns string into list of characters, then turns characters into digits
;; Finally make a 2D array with the result
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (mapcar 'digit-char-p (concatenate 'list s))) (get-file-lines input-file))))
    (make-array (list (length input) (length (CAR input))) :initial-contents input)))

;; Goes through each item in the array, and gets its neighbours. If a neighbour would be out-of-bounds,
;; set it to the value of the current position + 1. Then check that the current position is less than all
;; of it's neighbours and get that value + 1 if it is. Finally, sum all returned values.
(defun part-a (parsed-input)
  (apply '+ (loop with width = (CAR (array-dimensions parsed-input))
		  with height = (CADR (array-dimensions parsed-input))
		  for x from 0 below width
		  nconc (loop for y from 0 below height
			      for current = (aref parsed-input x y)
			      for left = (if (> 0 (- x 1)) (+ 1 current) (aref parsed-input (- x 1) y))
			      for right = (if (= width (+ 1 x)) (+ 1 current) (aref parsed-input (+ 1 x) y))
			      for above = (if (> 0 (- y 1)) (+ 1 current) (aref parsed-input x (- y 1)))
			      for below = (if (= height (+ 1 y)) (+ 1 current) (aref parsed-input x (+ 1 y)))
			      if (AND (< current left) (< current right) (< current above) (< current below)) collect (+ 1 current)))))

;; Start by getting the co-ordinates of all tiles in basins. Then take the first one, get all
;; of the neighbours in that basin and count how many there are. Repeat until all tiles are covered
;; then get the biggest 3 basins and multiply their sizes
(defvar cave-map)
(defun part-b (parsed-input)
  (setq cave-map (loop with width = (CAR (array-dimensions parsed-input))
	with height = (CADR (array-dimensions parsed-input))
	for x from 0 below width
	nconc (loop for y from 0 below height
		    if (/= 9 (aref parsed-input x y)) collect (cons x y))))
  (apply '*
	 (subseq (sort
		  (loop until (null cave-map)
			collect (length (count-adjacents (CAR cave-map))))
		  '>)
		 0 3)))

;; Get the list of tile co-ordinates adjacent to current, if none of them are in the basin list,
;; then the path is fully explored. Otherwise go through each adjacent tile in the basin and
;; check the tiles adjacent to them. Continue until all adjacents have been explored and combine their
;; co-ordinates with the current one's
(defun count-adjacents (current)
  (let* ((x (CAR current))
	 (y (CDR current))
	 (adjacents (list (cons (- x 1) y) (cons (+ x 1) y) (cons x (- y 1)) (cons x (+ y 1)))))
    (setq cave-map (remove current cave-map :test 'equal))
    (if (every 'null (mapcar (lambda (x) (find x cave-map)) adjacents)) current)
    (loop for v in adjacents
	  if (NOT (null (find v cave-map :test 'equal))) nconc (count-adjacents v) into results
	    finally (return (cons current results)))))
