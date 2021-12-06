;; Put the given solutions for the examples here
(setq test-sol-a 5934)
(setq test-sol-b 26984457539)

;; Split on , then map to integers, simple.
(defun parse-input (input-file)
  (setup (mapcar 'parse-integer (split "," (get-file-string input-file)))))

(defun part-a (parsed-input)
  (run-days parsed-input 80))

(defun part-b (parsed-input)
  (run-days parsed-input 256))

(defun setup (input)
  (let ((fish (circular (loop for i from 0 to 8 collect 0))))
    (loop for n in input do (incf (nth n fish)))
    fish))

;; For each day, move the list along 1 and add the removed number to the 7th position
(defun run-days (fish days)
  (loop for i from 0 below days
	for zeros = (pop fish)
	do (incf (nth 6 fish) zeros))
  (loop for i from 0 to 8 for f in fish sum f))

;; Turns a list into a circular list
(defun circular (items)
  (setf (CDR (last items)) items)
  items)
