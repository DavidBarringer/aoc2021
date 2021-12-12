;; Put the given solutions for the examples here
(setq test-sol-a 19)
(setq test-sol-b 103)

(defstruct edge
  (reachables nil)
  small)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (split "-" s)) (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (setup parsed-input)
  (length (move nil '|start| nil)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (setup parsed-input)
  (length (move-b nil '|start| nil nil)))

(defun setup (input)
  (loop for (e1 e2) in input
	for e1-small = (NOT (string= e1 (string-upcase e1)))
	for e2-small = (NOT (string= e2 (string-upcase e2)))
	do (setf (symbol-value (intern e1)) (make-edge :small e1-small))
	   (setf (symbol-value (intern e2)) (make-edge :small e2-small)))
  (loop for (e1 e2) in input do
    (if (NOT (string= "start" e1)) (push (intern e1) (edge-reachables (symbol-value (intern e2)))))
    (if (NOT (string= "start" e2)) (push (intern e2) (edge-reachables (symbol-value (intern e1)))))))
								     
(defun move (visited edge path)
  (if (edge-small (symbol-value edge)) (push edge visited))
  (if (equal edge '|end|) (list (reverse (cons edge path)))
      (loop for reachable in (edge-reachables (symbol-value edge))
	    if (NOT (member reachable visited)) nconc (move visited reachable (cons edge path)))))

(defun move-b (visited edge path visit-2)
  (let ((temp-visit-2 nil))
    (if (OR visit-2
	 (AND (edge-small (symbol-value edge)) (member edge visited) (NOT (equal edge '|start|)))) (setf temp-visit-2 t))
    (if (edge-small (symbol-value edge)) (push edge visited))
    (if (equal edge '|end|) (list (reverse (cons edge path)))
	(loop for reachable in (edge-reachables (symbol-value edge))
	      if (NOT (AND temp-visit-2 (member reachable visited))) nconc (move-b visited reachable (cons edge path) temp-visit-2)))))
