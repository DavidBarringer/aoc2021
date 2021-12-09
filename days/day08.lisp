;; Put the given solutions for the examples here
(setq test-sol-a 26)
(setq test-sol-b 61229)

(defstruct 7seg
  (one (lambda (x y) (CAR x)))
  (seven (lambda (x y) (CAR x)))
  (four (lambda (x y) (CAR x)))
  (eight (lambda (x y) (CAR (last x))))
  (six (lambda (x y) (find-diff 5 (7seg-one y) x)))
  (three (lambda (x y) (find-diff 3 (7seg-one y) x)))
  (nine (lambda (x y) (find-int 4 (7seg-four y) x)))
  (five (lambda (x y) (find-int 3 (7seg-four y) x)))
  (two (lambda (x y) (CAR x)))
  (zero (lambda (x y) (CAR x))))

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s)
	    (mapcar (lambda (s1)
		      (mapcar (lambda (s2) (sort (concatenate 'list s2) 'CHAR<)) (remove "" (split " " s1) :test 'string=)))
		    (split #\| s)))
	  (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for outputs in (mapcar 'CADR parsed-input)
	sum (loop for output in outputs if (member (length output) '(2 3 4 7)) count output)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (let ((7segs (loop for i in parsed-input
		     for digits = (sort (apply 'concatenate 'list i) '< :key 'length)
		     collect (to-7seg digits))))
    (loop for outputs in (mapcar 'CADR parsed-input)
	  for seg in 7segs
	  sum (parse-integer (format nil "~{~A~}" (loop for output in outputs collect (get-val output seg)))))))

(defun to-7seg (digits)
  (let ((result (make-7seg)))
    (loop for i in '(1 7 4 8 6 3 9 5 2 0)
	  for f = (read-from-string (format nil "7seg-~R" i))
	  do (funcall (fdefinition (list 'setf f)) (funcall (funcall f result) digits result) result)
	     (setq digits (remove (funcall f result) digits :test 'equal)))
    result))

(defun find-diff (target pred digits)
  (loop for d in digits
	if (= target (length (set-difference d pred))) return d))

(defun find-int (target pred digits)
  (loop for d in digits
	if (= target (length (intersection d pred))) return d))

(defun get-val (digits seg)
  (loop for i from 0 to 9
	for s = (funcall (read-from-string (format nil "7seg-~R" i)) seg)
	if (equal digits s) return i))
