;; Put the given solutions for the examples here
(setf test-sol-a 7)
(setf test-sol-b 5)

;; Turns lines into list, then converts to numbers, simple
(defun parse-input (input-file)
	(mapcar 'parse-integer (get-file-lines input-file)))

;; Loops through two item windows on input and counts each time the first is less than the second
(defun part-a (parsed-input)
	(loop for (prev x) on parsed-input until (null x) if (< prev x) count x))


;; Similar to part a, but with four item windows, comparing the sum of the first 3 with the next 3
(defun part-b (parsed-input)
	(loop for (w x y z) on parsed-input until (null z) if (< (+ w x y) (+ x y z)) count w))
