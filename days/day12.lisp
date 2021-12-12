;; Put the given solutions for the examples here
(setq test-sol-a 19)
(setq test-sol-b 103)

;; Make each cave a variable, with a list of caves it can travel to, and whether it is small
(defstruct cave
  (reachables nil)
  small)

;; Removes the - from the input, leaving pairs of caves
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (split "-" s)) (get-file-lines input-file))))
    (setup input)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (length (move nil '|start| nil)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (length (move-b nil '|start| nil nil)))

;; Goes through the parsed input and makes a cave of each string
;; Then re-goes through and adds to the list of reachables for each cave
(defun setup (input)
  (loop for (e1 e2) in input
	for e1-small = (NOT (string= e1 (string-upcase e1)))
	for e2-small = (NOT (string= e2 (string-upcase e2)))
	do (setf (symbol-value (intern e1)) (make-cave :small e1-small))
	   (setf (symbol-value (intern e2)) (make-cave :small e2-small)))
  (loop for (e1 e2) in input do
    (if (NOT (string= "start" e1)) (push (intern e1) (cave-reachables (symbol-value (intern e2)))))
    (if (NOT (string= "start" e2)) (push (intern e2) (cave-reachables (symbol-value (intern e1)))))))

;; If the cave is small, mark it as visited. If at the end of the path, return the path.
;; Otherwise, go through each reachable cave from current and if it's not in the visited list,
;; go to that cave and add the current one to the path
(defun move (visited cave path)
  (if (cave-small (symbol-value cave)) (push cave visited))
  (if (equal cave '|end|) (list (reverse (cons cave path)))
      (loop for reachable in (cave-reachables (symbol-value cave))
	    if (NOT (member reachable visited)) nconc (move visited reachable (cons cave path)))))

;; Similar to the previous, but if any small cave has been visited twice and the reachable has been
;; visited before then the reachable is discarded.
(defun move-b (visited cave path visit-2)
  (let ((temp-visit-2 nil))
    (if (OR visit-2
	 (AND (cave-small (symbol-value cave)) (member cave visited) (NOT (equal cave '|start|)))) (setf temp-visit-2 t))
    (if (cave-small (symbol-value cave)) (push cave visited))
    (if (equal cave '|end|) (list (reverse (cons cave path)))
	(loop for reachable in (cave-reachables (symbol-value cave))
	      if (NOT (AND temp-visit-2 (member reachable visited))) nconc (move-b visited reachable (cons cave path) temp-visit-2)))))
