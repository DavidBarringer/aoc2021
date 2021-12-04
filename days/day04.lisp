;; Put the given solutions for the examples here
(setq test-sol-a 4512)
(setq test-sol-b 1924)

;; Step 1: split on double new lines
;; Step 2: the first element gets split by commas
;; Step 3a: the rest are bingo cards, they get split by new lines
;; Step 3b: then they are split by spaces, removing any empty strings
;; Step 3c: the numbers are then stored as variables set to NIL
(defun parse-input (input-file)
  (let ((input (split (format nil "~%~%") (get-file-string input-file))))
    (cons (split "," (CAR input))
	  (mapcar (lambda (r)
		    (mapcar (lambda (c)
			      (loop for s in (remove "" (split " " c) :test 'string=)
				    do (setf (symbol-value (intern s)) nil)
				    collect s))
			    (split #\Newline r)))
		  (CDR input)))))

;; For each number called out, change its variable to T, then check each card to see if it is filled.
;; When a card position is returned the last number called multiplied by the score of that card
(defun part-a (parsed-input)
  (loop for n in (CAR parsed-input) do (setf (symbol-value (intern n)) t)
				       (let ((check (check (CDR parsed-input))))
					 (if (NOT (null check)) (return (* (parse-integer n) (get-score (nth (CAR check) (CDR parsed-input)))))))))

;; Firstly, set all variables back to NIL. Then set a local variable of each of the bingo cards.
;; Then go through numbers similar to part 1, but when the check succeeds only return if it was the last card.
;; Otherwise, remove any filled cards from the cards variable (order of filled cards is reversed to prevent index fuckery)
(defun part-b (parsed-input)
  (loop for n in (CAR parsed-input) do (setf (symbol-value (intern n)) nil))
  (let ((cards (CDR parsed-input)))
    (loop for n in (CAR parsed-input)
	  do (setf (symbol-value (intern n)) t)
	     (let ((check (check cards)))
	       (COND
		 ((AND (NOT (null check)) (= 1 (length cards))) (return (* (parse-integer n) (get-score (CAR cards)))))
		 ((NOT (null check)) (loop for c in (reverse check) do (setf cards (remove (nth c cards) cards)))))))))

;; First, two variables are created, one showing the bingo cards, but only whether a number has been seen;
;; the second is a transposition of the first, to make checking the columns easier.
;; Then go through each bingo card, and go through each row and column, if none of the values in a row or column are NIL,
;; then the card is complete and added to the list. Duplicates are removed as a row and column can be completed at the same time
(defun check (cards)
  (let* ((check (mapcar (lambda (card) (mapcar (lambda (row) (mapcar (lambda (col) (symbol-value (intern col))) row)) card)) cards))
	 (check-t (mapcar (lambda (card) (apply 'mapcar 'list card)) check)))
    (remove-duplicates (loop for i from 0 for card in check for card-t in check-t
			     nconc (loop for row in card for col in card-t if (OR (notany 'null row) (notany 'null col)) collect i)))))

;; Go through each number in the card, if its variable is NIL then add its value to the sum
(defun get-score (card)
  (loop for row in card
	sum (loop for col in row if (null (symbol-value (intern col))) sum (parse-integer col))))

