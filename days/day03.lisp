;; Put the given solutions for the examples here
(setq test-sol-a 198)
(setq test-sol-b 230)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (x) (mapcar 'digit-char-p (concatenate 'list x))) (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (apply '* (mapcar (lambda (x) (parse-integer (format nil "~{~A~}" x) :radix 2)) (get-greatest-least-bits parsed-input))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (* (parse-integer (format nil "~{~A~}" (filter-02 (CAR (get-greatest-least-bits parsed-input)) parsed-input 1)) :radix 2)
     (parse-integer (format nil "~{~A~}" (filter-02 (CADR (get-greatest-least-bits parsed-input)) parsed-input 0)) :radix 2)))

(defun get-greatest-least-bits (input)
  (if (null input) (list nil nil)
      (loop for x in (apply 'mapcar 'list input)
	    if (>= (count 1 x) (count 0 x))
	      collect 1 into greatest and collect 0 into least
	    else collect 0 into greatest and collect 1 into least
	    finally (return (list greatest least)))))

(defun filter-02 (filter range prio)
  (cons (CAR filter)
	(remove nil (loop for f in filter for x = (print (format nil "Filter: ~A" filter)) for y = (print range) until (null filter)
			   nconc
			   (loop for r in range if (= f (CAR r)) collect (CDR r) into next
				 finally
				    (COND ((= 1 (length next)) (setf filter nil)
					   (return (CAR next)))
					  (t
					   (setf range next)
					   (setf filter (if (= prio 1) (CAR (get-greatest-least-bits next))
							    (CADR (get-greatest-least-bits next))))
					   (return (list (CAR filter))))))))))
