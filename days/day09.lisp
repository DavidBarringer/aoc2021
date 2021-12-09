;; Put the given solutions for the examples here
(setq test-sol-a 15)
(setq test-sol-b 1134)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (mapcar 'digit-char-p (concatenate 'list s))) (get-file-lines input-file))))
    (make-array (list (length input) (length (CAR input))) :initial-contents input)))

;; Returns the solution for part a
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

;; Returns the solution for part b
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

(defun count-adjacents (current)
  (let* ((x (CAR current))
	 (y (CDR current))
	 (adjacents (list (cons (- x 1) y) (cons (+ x 1) y) (cons x (- y 1)) (cons x (+ y 1)))))
    (setq cave-map (remove current cave-map :test 'equal))
    (if (every 'null (mapcar (lambda (x) (find x cave-map)) adjacents)) current)
    (loop for v in adjacents
	  if (NOT (null (find v cave-map :test 'equal))) nconc (count-adjacents v) into results
	    finally (return (cons current results)))))
