;; Put the given solutions for the examples here
(setf test-sol-a 7)
(setf test-sol-b 5)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
	(mapcar 'parse-integer (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
	(loop for (prev x) on parsed-input until (null x) if (< prev x) count x))


;; Returns the solution for part b
(defun part-b (parsed-input)
	(loop for (w x y z) on parsed-input until (null z) if (< (+ w x y) (+ x y z)) count w))
