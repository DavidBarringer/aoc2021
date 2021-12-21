;; Put the given solutions for the examples here
(setq test-sol-a 45)
(setq test-sol-b 112)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (i) (parse-integer i :junk-allowed t)) (CDR (apply 'nconc (mapcar (lambda (s) (split "=" s)) (split "\\.\\." (get-file-string input-file)))))))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (/ (* (- 0 (nth 2 parsed-input) 1) (- 0 (nth 2 parsed-input))) 2))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for x from 0 to (CADR parsed-input)
	sum (loop for y from (CADDR parsed-input) to (- 0 (nth 2 parsed-input) 1)
		  count (reaches parsed-input x y))))

(defun reaches (bounds x-vel y-vel)
  (let ((x-pos 0)
	(y-pos 0))
    (loop until (OR (> x-pos (CADR bounds)) (< y-pos (CADDR bounds)))
	  if (AND (>= x-pos (CAR bounds)) (<= y-pos (CADDDR bounds)))
	    return t
	  else
	    do (setf x-pos (+ x-pos x-vel))
	       (setf y-pos (+ y-pos y-vel))
	       (setf x-vel (if (= 0 x-vel) 0 (- x-vel 1)))
	       (setf y-vel (- y-vel 1)))))
