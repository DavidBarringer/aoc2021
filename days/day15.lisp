;; Put the given solutions for the examples here
(setq test-sol-a 40)
(setq test-sol-b 315)

(defstruct heap
  (pointer 0)
  (array (make-array 5000 :initial-element nil)))

(defstruct node
  (value 0)
  path)

(defun insert (heap value)
  (incf (heap-pointer heap))
  (if (= (heap-pointer heap) 1)
      (setf (aref (heap-array heap) 1) value)
      (let ((index (ignore-errors (loop for i from 1 below (heap-pointer heap)
					if (equal (node-path value) (node-path (aref (heap-array heap) i)))
					  do (return i)))))
	(if index
	    (if (< (node-value value) (node-value (aref (heap-array heap) index)))
		(progn (setf (aref (heap-array heap) index) value)
		       (setf heap (sift-up heap index))))
	    (progn (setf (aref (heap-array heap) (heap-pointer heap)) value)
		   (setf heap (sift-up heap (heap-pointer heap)))))
	(if index (decf (heap-pointer heap)))))
  heap)

(defun sift-up (heap index)
  (loop with i = index
	with value = (aref (heap-array heap) index)
	for parent = (aref (heap-array heap) (floor (/ i 2)))
	until (OR (= i 1) (> (node-value value) (node-value parent)))
	do (setf (aref (heap-array heap) i) parent)
	   (setf (aref (heap-array heap) (floor (/ i 2))) value)
	   (setf i (floor (/ i 2))))
  heap)

(defun remove-head (heap)
  (setf (aref (heap-array heap) 0) (aref (heap-array heap) 1))
  (ignore-errors
   (loop initially (setf (aref (heap-array heap) 1) (aref (heap-array heap) (heap-pointer heap)))
		   (setf (aref (heap-array heap) (heap-pointer heap)) nil)
		   (decf (heap-pointer heap))
	 with i = 1
	 for left = (aref (heap-array heap) (* 2 i))
	 for right = (aref (heap-array heap) (+ 1 (* 2 i)))
	 for min = (CAR (sort (remove nil (list (aref (heap-array heap) i) left right)) '< :key 'node-value))
	 until (OR (null left) (equal min (aref (heap-array heap) i)))
	 do (if (equal min right) 
		(progn (setf (aref (heap-array heap) (+ 1 (* 2 i))) (aref (heap-array heap) i))
		       (setf (aref (heap-array heap) i) min)
		       (setf i (+ 1 (* 2 i))))
		(progn (setf (aref (heap-array heap) (* 2 i)) (aref (heap-array heap) i))
		       (setf (aref (heap-array heap) i) min)
		       (setf i (* 2 i)))))
   (aref (heap-array heap) 0)))

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (mapcar 'digit-char-p (concatenate 'list s))) (get-file-lines input-file))))
    (make-array (list (length input) (length (CAR input))) :initial-contents input)))


(defun neighbours (current width height)
  (let ((path (node-path current)))
    (loop for x from -1 to 1
	  with coord = path
	  if (AND (<= 0 (+ x (CAR coord)))
		  (> width (+ x (CAR coord))))
	    nconc (loop for y from -1 to 1
			if (AND (NOT (= x y))
				(NOT (= (* -1 x) y))
				(<= 0 (+ y (CDR coord)))
				(> height (+ y (CDR coord))))
			  collect (cons (+ x (CAR coord)) (+ y (CDR coord)))))))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (let ((heap (make-heap))
	(visited nil)
	(width (CAR (array-dimensions parsed-input)))
	(height (CADR (array-dimensions parsed-input))))
    (insert heap (make-node :value 0 :path '(0 . 0)))
    (loop for current = (remove-head heap)
	  until (equal (node-path current) (cons (- width 1) (- height 1)))
	  if (NOT (member (node-path current) visited :test 'equal))
	  do (loop for (x . y) in (neighbours current width height)
		   if (NOT (member (cons x y) visited :test 'equal))
		     do (insert heap (make-node :value (+ (node-value current) (aref parsed-input x y)) :path (cons x y))))
	     (push (node-path current) visited))
    (node-value (aref (heap-array heap) 0))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (let ((expanded-map (make-array (mapcar (lambda (d) (* 5 d)) (array-dimensions parsed-input))))
	(width (- (CAR (array-dimensions parsed-input)) 1))
	(height (- (CADR (array-dimensions parsed-input)) 1)))
    (loop for x-major from 0 below 5
	  do (loop for y-major from 0 below 5
		   do (loop for x from 0 to width
			    do (loop for y from 0 to height
				     for new-val = (+ (aref parsed-input x y) (* (+ x-major y-major) 1))
				     do (setf (aref expanded-map (+ (* x-major (+ width 1)) x) (+ (* y-major (+ height 1)) y))
					      (if (< 9 new-val) (mod new-val 9) new-val))))))
    (let ((heap (make-heap))
	  (visited nil)
	  (width (CAR (array-dimensions expanded-map)))
	  (height (CADR (array-dimensions expanded-map))))
      (insert heap (make-node :value 0 :path '(0 . 0)))
      (loop for current = (remove-head heap)
	    until (equal (node-path current) (cons (- width 1) (- height 1)))
	    if (NOT (member (node-path current) visited :test 'equal))
	      do (loop for (x . y) in (neighbours current width height)
		       if (NOT (member (cons x y) visited :test 'equal))
			 do (insert heap (make-node :value (+ (node-value current) (aref expanded-map x y)) :path (cons x y))))
		 (push (node-path current) visited))
      (node-value (aref (heap-array heap) 0)))))

