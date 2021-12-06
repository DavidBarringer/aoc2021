;; Put the given solutions for the examples here
(setq test-sol-a 5934)
(setq test-sol-b 26984457539)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (setup (mapcar 'parse-integer (split "," (get-file-string input-file)))))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (run-days parsed-input 80))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (run-days parsed-input 256))

(defun setup (input)
  (let ((fish (circular (loop for i from 0 to 8 collect 0))))
    (loop for n in input do (incf (nth n fish)))
    fish))

(defun run-days (fish days)
  (loop for i from 0 below days
	for zeros = (pop fish)
	do (incf (nth 6 fish) zeros))
  (loop for i from 0 to 8 for f in fish sum f))

(defun circular (items)
  (setf (CDR (last items)) items)
  items)
