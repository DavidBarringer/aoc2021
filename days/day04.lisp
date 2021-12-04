;; Put the given solutions for the examples here
(setq test-sol-a 4512)
(setq test-sol-b 1924)

(defstruct (num)
  seen)

;; Err
(defun parse-input (input-file)
  (let ((input (split (format nil "~%~%") (get-file-string input-file))))
    (cons (split "," (CAR input))
	  (mapcar (lambda (r)
		    (mapcar (lambda (c)
			      (loop for s in (remove "" (split " " c) :test 'string=)
				    do (setf (symbol-value (intern s)) (make-num :seen nil))
				    collect s))
			    (split #\Newline r)))
		  (CDR input)))))

;; Hmm
(defun part-a (parsed-input)
  (loop for n in (CAR parsed-input) do (setf (num-seen (symbol-value (intern n))) t)
				       (let ((check (check (CDR parsed-input))))
					 (if (NOT (null check)) (return (* (parse-integer n) (get-score (nth (CAR check) (CDR parsed-input)))))))))

;; :thinking:
(defun part-b (parsed-input)
  (loop for n in (CAR parsed-input) do (setf (num-seen (symbol-value (intern n))) nil))
  (let ((cards (CDR parsed-input)))
    (loop for n in (CAR parsed-input)
	  do (setf (num-seen (symbol-value (intern n))) t)
	     (let ((check (check cards)))
	       (COND
		 ((AND (NOT (null check)) (= 1 (length cards))) (return (* (parse-integer n) (get-score (CAR cards)))))
		 ((NOT (null check)) (loop for c in (reverse check) do (setf cards (remove (nth c cards) cards)))))))))

;; :eyes:
(defun check (cards)
  (let* ((check (mapcar (lambda (card) (mapcar (lambda (row) (mapcar (lambda (col) (get-seen col)) row)) card)) cards))
	 (check-t (mapcar (lambda (card) (apply 'mapcar 'list card)) check)))
    (remove-duplicates (loop for i from 0 for card in check for card-t in check-t
			     nconc (loop for row in card for col in card-t if (OR (notany 'null row) (notany 'null col)) collect i)))))

;; ...
(defun get-score (card)
  (loop for row in card
	sum (loop for col in row if (null (get-seen col)) sum (parse-integer col))))

(defun get-seen (val)
  (num-seen (symbol-value (intern val))))
