;; Put the given solutions for the examples here
(setq test-sol-a 37)
(setq test-sol-b 168)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar 'parse-integer (split "," (get-file-string input-file))))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for n from 0 to (apply 'max parsed-input)
	minimize (apply '+ (mapcar (lambda (x) (abs (- n x))) parsed-input))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for n from 0 to (apply 'max parsed-input)
	minimize (apply '+ (mapcar (lambda (x) (/ (* (abs (- n x)) (+ 1 (abs (- n x)))) 2)) parsed-input))))
