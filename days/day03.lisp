;; Put the given solutions for the examples here
(setq test-sol-a 198)
(setq test-sol-b 230)

;; Reads each line in, changes string to list of characters, then turns them into a list of digits
(defun parse-input (input-file)
  (mapcar (lambda (x) (mapcar 'digit-char-p (concatenate 'list x))) (get-file-lines input-file)))

;; (format nil "~{~A~}" x) turns a list into a spaceless string, then (parse-integer x :radix 2) reads it from binary to decimal
;; then multiply the greatest result with the littlest
(defun part-a (parsed-input)
  (apply '* (mapcar (lambda (x) (parse-integer (format nil "~{~A~}" x) :radix 2)) (get-greatest-least-bits parsed-input))))

;; Similar combining function to part a, but greatest/littlest are handled seperately so theres no need for (apply)
(defun part-b (parsed-input)
  (* (parse-integer (format nil "~{~A~}" (filter-bits parsed-input 1)) :radix 2)
     (parse-integer (format nil "~{~A~}" (filter-bits parsed-input 0)) :radix 2)))

;; (apply 'mapcar 'list 'input) makes a list of the bits in each position (a transposition)
;; the most common bit is put in the "greatest" result, least common goes in "least"
;; tiebreaker is always 1 into greatest, 0 into least
(defun get-greatest-least-bits (input)
  (loop for x in (apply 'mapcar 'list input)
	if (<= (count 0 x) (count 1 x))
	  collect 1 into greatest and collect 0 into least
	else collect 0 into greatest and collect 1 into least
	finally (return (list greatest least))))

;; Holy water advised:
;; Get the first filter same as part a. If oxygen, then use "greatest", else use "least"
;; The inner loop will check that the bit in the position matches that of the filter
;; if it doesn't the value is removed from range. Once this is done for all values,
;; the filter gets changed based on the resulting range, when the range has only one
;; value left, return it.
(defun filter-bits (range prio)
  (let ((filter (if (= prio 1) (CAR (get-greatest-least-bits range))
		    (CADR (get-greatest-least-bits range)))))
    (loop for i from 0 for f = (nth i filter)
	  do (loop for r in range
		   if (/= f (nth i r)) do (setf range (remove r range))
		     finally (setf filter (if (= prio 1) (CAR (get-greatest-least-bits range))
					      (CADR (get-greatest-least-bits range)))))
	  when (= 1 (length range))
	    do (return (CAR range)))))
