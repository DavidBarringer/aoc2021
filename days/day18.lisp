;; Put the given solutions for the examples here
(setf test-sol-a 4140)
(setf test-sol-b 3993)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (read-from-string (substitute #\( #\[ (substitute #\) #\] (substitute #\Space #\, s)))))
	    (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (shmagnitude (shum parsed-input)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for nums on parsed-input
	for num1 = (CAR nums)
	maximize (loop for num2 in (CDR nums)
		       maximize (max (shmagnitude (shum (list num1 num2)))
				    (shmagnitude (shum (list num2 num1)))))))

(defun shum (input)
  (loop with res = (CAR input)
	for next in (CDR input)
	do (setf res (shreduce (list res next)))
	finally (return res)))

(defun shmagnitude (shnum)
  (if (numberp shnum) shnum
      (+ (* 3 (shmagnitude (CAR shnum))) (* 2 (shmagnitude (CADR shnum))))))

(defun exp-build (pair left right)
  (loop with res = (list (add (CAR left) (CAR pair) (CADR pair)) (add (CAR right) (CADR pair) (CADR pair)))
	with l-comp = (CAR left)
	with r-comp = (CAR right)
	for l in (CDR left)
	for r in (CDR right)
	if l
	  do (if (NOT l-comp) (progn (setf res (list (add l (CAR pair) nil) res))
				       (setf l-comp t))
		 (setf res (list l res)))
	else if r
	       do (if (NOT r-comp) (progn (setf res (list res (add r (CADR pair) t)))
					    (setf r-comp t))
		      (setf res (list res r)))
	finally
	   (return res)))

(defun split-build (pair left right)
  (loop with res = pair
	for l in left
	for r in right
	if l
	  do (setf res (list l res))
	else if r
	       do (setf res (list res r))
	finally
	   (return res)))

(defun explode-pair (depth shnum left right)
  (if (= depth 4) (apply 'exp-build (list shnum left right))
      (OR (if (listp (CAR shnum)) (explode-pair (+ 1 depth) (CAR shnum) (cons nil left) (cons (CADR shnum) right)) nil)
	  (if (listp (CADR shnum)) (explode-pair (+ 1 depth) (CADR shnum) (cons (CAR shnum) left) (cons nil right)) nil))))

(defun split-num (shnum left right)
  (if (AND (numberp shnum) (< 9 shnum)) (apply 'split-build (list (list (floor (/ shnum 2)) (ceiling (/ shnum 2))) left right))
      (if (listp shnum)
	  (OR
	   (split-num (CAR shnum) (cons nil left) (cons (CADR shnum) right))
	   (split-num (CADR shnum) (cons (CAR shnum) left) (cons nil right)))
	  nil)))

(defun add (res num r)
  (COND
    ((null res) 0)
    ((numberp res) (+ res num))
    (r (cons (add (CAR res) num r) (CDR res)))
    ((numberp (CADR res)) (list (CAR res) (+ (CADR res) num)))
    (t (cons (CAR res) (list (add (CADR res) num r))))))

(defun shreduce (shnum)
  (loop with res = shnum
	for next = (OR (explode-pair 0 res nil nil)
		       (split-num res nil nil))
	until (null next)
	do (setf res next)
	finally (return res)))
