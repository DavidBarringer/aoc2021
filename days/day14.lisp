;; Put the given solutions for the examples here
(setq test-sol-a 1588)
(setq test-sol-b 2188189693529)

(defstruct pair
  result
  next)

(defun setup (pairs initial)
  (let ((elements nil))
    (loop for pair in pairs do (setf (symbol-value (intern (CAR pair)))
				     (make-pair :result (intern (CADR pair))
						:next (list (intern (format nil "~A~A" (char (CAR pair) 0) (CADR pair)))
							    (intern (format nil "~A~A" (CADR pair) (char (CAR pair) 1))))))
			       (setf (symbol-value (intern (CADR pair))) 0)
			       (push (intern (CADR pair)) elements))
    (let ((polys (make-hash-table)))
      (loop for (s1 s2) on (concatenate 'list initial)
	    until (null s2)
	    for poly = (intern (format nil "~A~A" s1 s2))
	    do (if (gethash poly polys)
		   (incf (gethash poly polys))
		   (setf (gethash poly polys) 1)))
      (cons polys elements))))

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let* ((input (split (format nil "~%~%") (get-file-string input-file)))
	 (pairs (mapcar (lambda (s) (split " -> " s)) (split #\Newline (CADR input)))))
    (setup pairs (CAR input))))

(defun expand-pairs (polys depth)
  (loop for i from 1 to depth do
    (let ((new-polys (make-hash-table)))
      (loop for pair being the hash-keys of polys
	      using (hash-value count)
	    for result = (pair-result (symbol-value pair))
	    do (incf (symbol-value result) count)
	       (loop for n in (pair-next (symbol-value pair))
		     do (if (gethash n new-polys)
			    (incf (gethash n new-polys) count)
			    (setf (gethash n new-polys) count))))
      (setf polys new-polys)))
  polys)


;; Returns the solution for part a
(defun part-a (parsed-input)
  (expand-pairs (CAR parsed-input) 10)
  (loop for v in (mapcar 'symbol-value (CDR parsed-input))
	maximize v into max
	minimize v into min
	finally (return (+ 1 (- max min)))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (expand-pairs (CAR parsed-input) 40)
  (loop for v in (mapcar 'symbol-value (CDR parsed-input))
	maximize v into max
	minimize v into min
	finally (return (+ 1 (- max min)))))
