;; Put the given solutions for the examples here
(setq test-sol-a 26)
(setq test-sol-b 61229)

;; A structure that has default values of the functions that will be used to
;; obtain the actual values. The functions have the same form so that they can
;; be run as a part of a loop
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

;; Split on |, then split on spaces, removing empty strings. Then turn into list of characters
(defun parse-input (input-file)
  (mapcar (lambda (s)
	    (mapcar (lambda (s1)
		      (mapcar (lambda (s2) (sort (concatenate 'list s2) 'CHAR<)) (remove "" (split " " s1) :test 'string=)))
		    (split #\| s)))
	  (get-file-lines input-file)))

;; For each output, count the number of displays that have a length of 2, 3, 4 or 7. Then sum
;; the results of each output
(defun part-a (parsed-input)
  (loop for outputs in (mapcar 'CADR parsed-input)
	sum (loop for output in outputs if (member (length output) '(2 3 4 7)) count output)))

;; Make a list of 7seg structs to use for each output, and map the actual values using the
;; digits from that display (sorted for use in mapping). Then go through each output and its
;; corresponding 7seg struct to get the number value, summing the results of each output.
(defun part-b (parsed-input)
  (let ((7segs (loop for i in parsed-input
		     for digits = (sort (apply 'concatenate 'list i) '< :key 'length)
		     collect (to-7seg digits))))
    (loop for outputs in (mapcar 'CADR parsed-input)
	  for seg in 7segs
	  sum (parse-integer (format nil "~{~A~}" (loop for output in outputs collect (get-val output seg)))))))

;; Goes through the numbers of the 7seg display (in the odd order given) and runs the
;; respective function in the structure to get the necessary values. Then removes that
;; digit from the list.
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

;; Goes through each digit in the given 7seg struct and checks if it matches the
;; given digit, returning the matching number
(defun get-val (digits seg)
  (loop for i from 0 to 9
	for s = (funcall (read-from-string (format nil "7seg-~R" i)) seg)
	if (equal digits s) return i))
