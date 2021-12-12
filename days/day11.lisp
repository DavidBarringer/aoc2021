;; Put the given solutions for the examples here
(setq test-sol-a 1656)
(setq test-sol-b 195)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (mapcar 'digit-char-p (concatenate 'list s))) (get-file-lines input-file))))
    (make-array (list (length input) (length (CAR input))) :initial-contents input)))

(defun neighbours (curr-x curr-y width height)
  (remove (cons curr-x curr-y)
	  (loop for x from -1 to 1
		nconc (loop for y from -1 to 1
			    if (AND (>= (+ curr-x x) 0)
				    (< (+ curr-x x) width)
				    (>= (+ curr-y y) 0)
				    (< (+ curr-y y) height))
			      collect (cons (+ curr-x x) (+ curr-y y)))) :test 'equal))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for j from 1 to 100 do
    (loop for i from 0 below (array-total-size parsed-input) do (incf (row-major-aref parsed-input i)))
    sum
    (loop with width = (CAR (array-dimensions parsed-input))
	with height = (CADR (array-dimensions parsed-input))
	for x from 0 below width
	sum 
	(loop for y from 0 below height
		 if (< 9 (aref parsed-input x y))
		   do (setf (aref parsed-input x y) 0)
		      (loop for (xn . yn) in (neighbours x y width height)
			    do (if (/= 0 (aref parsed-input xn yn)) (incf (aref parsed-input xn yn)))
			       (if (= 10 (aref parsed-input xn yn))
				   (progn
				     (setf x (if (= 0 (min x xn)) 0 (- (min x xn) 1)))
				     (setf y (if (> 0 (min y yn)) -1 (- (min y yn) 1))))))
		      and count y into flashes
		 finally (return flashes)))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for j from 1 do
    (loop for i from 0 below (array-total-size parsed-input) do (incf (row-major-aref parsed-input i)))
    if (= (array-total-size parsed-input)
    (loop with width = (CAR (array-dimensions parsed-input))
	with height = (CADR (array-dimensions parsed-input))
	for x from 0 below width
	sum 
	(loop for y from 0 below height
		 if (< 9 (aref parsed-input x y))
		   do (setf (aref parsed-input x y) 0)
		      (loop for (xn . yn) in (neighbours x y width height)
			    do (if (/= 0 (aref parsed-input xn yn)) (incf (aref parsed-input xn yn)))
			       (if (= 10 (aref parsed-input xn yn))
				   (progn
				     (setf x (if (= 0 (min x xn)) 0 (- (min x xn) 1)))
				     (setf y (if (> 0 (min y yn)) -1 (- (min y yn) 1))))))
		      and count y into flashes
	      finally (return flashes))))
      return j))
