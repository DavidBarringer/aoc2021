;; Put the given solutions for the examples here
(setq test-sol-a 26397)
(setq test-sol-b 288957)

(defun setup ()
  (loop for bracket in '(#\) #\] #\} #\> #\< #\{ #\[ #\()
	for matching in '(#\( #\[ #\{ #\< #\> #\{ #\] #\))
	for score in '(3 57 1197 25137 4 3 2 1)
	for name = (format nil "B-~A" bracket)
	do (setf (symbol-value (intern name)) (make-bracket :matching matching :score score))))

(defstruct bracket
  score
  matching)

(setup)

;; Turn each line into a list of characters
(defun parse-input (input-file)
  (mapcar (lambda (s) (concatenate 'list s)) (get-file-lines input-file)))

;; For each line, keep a stack of opening bracket styles, if a closing bracket is seen and matches the opening one
;; remove the opening from the stack, otherwise, there is an error, and return the score for that bracket
(defun part-a (parsed-input)
  (let ((opens '(#\( #\[ #\{ #\<)))
    (apply '+ (remove nil
		      (loop for input in parsed-input
			    for stack = nil
			    collect (loop for c in input
					  for name = (format nil "B-~A" c)
					  if (member c opens) do (push c stack)
					    else if (equal (bracket-matching (symbol-value (intern name))) (CAR stack)) do (pop stack)
						   else do (return (bracket-score (symbol-value (intern name))))))))))


;; Similar to part a, but don't return errors, instead return the remaining stack when there are no errors, and get their score
;; sum the scores of each opening bracket. Then find the middle value.
(defun part-b (parsed-input)
  (let* ((opens '(#\( #\[ #\{ #\<))
	 (results (remove nil
			  (loop for input in parsed-input
				for stack = nil
				collect (loop for c in input
					      for name = (format nil "B-~A" c)
					      if (member c opens) do (push c stack)
						else if (equal (bracket-matching (symbol-value (intern name))) (CAR stack)) do (pop stack)
						       else collect t into err
					      finally (if (NOT (CAR err))
							  (return (mapcar (lambda (s) (bracket-score (symbol-value (intern (format nil "B-~A" s))))) stack)))))))
	 (scores (sort (loop for r in results
			     collect (loop with score = 0
					   for value in r
					   do (setf score (+ value (* 5 score)))
					   finally (return score))) '<)))
    (nth (/ (- (length scores) 1) 2) scores)))


