;; Put the given solutions for the examples here
(setq test-sol-a 150)
(setq test-sol-b 900)

(defun parse-input (input-file)
	(let ((input (mapcar (lambda (s) (split " " s)) (get-file-lines input-file))))
		(mapcar (lambda (x) (cons (intern (string-upcase (CAR x))) (parse-integer (CADR x)))) input)))

(defun part-a (parsed-input)
	(let ((forward 0) (up 0) (down 0))
		(loop for x in parsed-input do (set (CAR x) (+ (eval (CAR x)) (CDR x))))
		(* forward (- down up))))

(defun part-b (parsed-input)
	(let ((forward 0) (depth 0) (up 0) (down 0))
		(loop for x in parsed-input do
			(set (CAR x) (+ (eval (CAR x)) (CDR x)))
			(if (eq (CAR x) 'forward) (setf depth (+ depth (* (CDR x) (- down up))))))
		(* forward depth)))
