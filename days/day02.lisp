;; Put the given solutions for the examples here
(setq test-sol-a 150)
(setq test-sol-b 900)

;; Input becomes list of instructions first map turns each instruction into it's own list
;; second map takes the first character, parses the integer and makes the instruction a dotted pair e.g (#\f . 2)
(defun parse-input (input-file)
	(let ((input (mapcar (lambda (s) (split " " s)) (get-file-lines input-file))))
		(mapcar (lambda (x) (cons (char (CAR x) 0) (parse-integer (CADR x)))) input)))

(defun part-a (parsed-input)
	(move parsed-input 0 0))

(defun part-b (parsed-input)
	(move-b parsed-input 0 0 0))

(defun move (instructions forward depth)
	(COND
		((null instructions) (* forward depth))
		((eq (CAAR instructions) #\f) (move (CDR instructions) (+ forward (CDAR instructions)) depth))
		((eq (CAAR instructions) #\u) (move (CDR instructions) forward (- depth (CDAR instructions))))
		(t (move (CDR instructions) forward (+ depth (CDAR instructions))))))

(defun move-b (instructions forward depth aim)
	(COND
		((null instructions) (* forward depth))
		((eq (CAAR instructions) #\f) (move-b (CDR instructions) (+ forward (CDAR instructions)) (+ depth (* aim (CDAR instructions))) aim))
		((eq (CAAR instructions) #\u) (move-b (CDR instructions) forward depth (- aim (CDAR instructions))))
		(t (move-b (CDR instructions) forward depth (+ aim (CDAR instructions))))))
