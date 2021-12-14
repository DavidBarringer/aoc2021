;; Put the given solutions for the examples here
(setq test-sol-a 17)
(setq test-sol-b "A square above two empty lines")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let* ((state (mapcar (lambda (s) (apply 'cons (mapcar 'parse-integer (split "," s)))) (split #\Newline (CAR (split (format nil "~%~%") (get-file-string input-file))))))
	 (instructions (mapcar (lambda (s) (cons (char (CAR (split "=" (subseq s 11))) 0) (parse-integer (CADR (split "=" (subseq s 11)))))) (split #\Newline (CADR (split (format nil "~%~%") (get-file-string input-file))))))
	 (paper (make-array (list (+ 1 (CDR (maxes state))) (+ 1 (CAR (maxes state)))) :initial-element " ")))
    (loop for coord in state do (setf (aref paper (CDR coord) (CAR coord)) "█"))
    (list paper instructions)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (let ((result (if (eq #\y (CAAADR parsed-input)) (fold-y (CAR parsed-input) (CDAADR parsed-input))
		    (fold-x (CAR parsed-input) (CDAADR parsed-input)))))
    (loop for i from 0 below (array-total-size result) if (string= (row-major-aref result i) "█") count i)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (let ((result (CAR parsed-input)))
    (loop for (dir . pos) in (CADR parsed-input)
	  if (eq #\y dir) do (setf result (fold-y result pos))
	    else do (setf result (fold-x result pos)))
    (format nil "~%~{~{~A~}~%~}" (loop for x from 0 below (CAR (array-dimensions result))
				       collect (loop for y from 0 below (CADR (array-dimensions result)) collect (aref result x y))))))

(defun maxes (coords)
  (loop for (x . y) in coords
  maximize x into xmax
  maximize y into ymax
  finally (return (cons xmax ymax))))

(defun fold-y (paper instruction)
  (let ((new-paper (make-array (list instruction (CADR (array-dimensions paper))) :initial-element " ")))
    (loop for x from 0 below (CAR (array-dimensions new-paper)) do
      (loop for y from 0 below (CADR (array-dimensions new-paper))
	    if (OR (string= (aref paper x y) "█") (string= (aref paper (- (CAR (array-dimensions paper)) x 1) y) "█"))
	      do (setf (aref new-paper x y) "█")))
    new-paper))

(defun fold-x (paper instruction)
  (let ((new-paper (make-array (list (CAR (array-dimensions paper)) instruction) :initial-element " ")))
    (loop for x from 0 below (CAR (array-dimensions new-paper)) do
      (loop for y from 0 below (CADR (array-dimensions new-paper))
	    if (OR (string= (aref paper x y) "█") (string= (aref paper x (- (CADR (array-dimensions paper)) y 1)) "█"))
	      do (setf (aref new-paper x y) "█")))
    new-paper))
      
