;; Put the given solutions for the examples here
(setq test-sol-a 150)
(setq test-sol-b 900)

(defvar forward)
(defvar up)
(defvar down)

;; First map makes a list (direction magnitude) for each line
;; Second map turns the direction into a symbol and the magnitude into an int and makes them a dotted pair
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (split " " s)) (get-file-lines input-file))))
    (mapcar (lambda (x) (cons (intern (string-upcase (CAR x))) (parse-integer (CADR x)))) input)))

;; For each instruction in the list, get the variable name and set the value to prev value + magnitude.
;; At the end, subtract the up from the down to get total depth change, then multiply by forward
(defun part-a (parsed-input)
  (let ((forward 0) (up 0) (down 0))
    (loop for x in parsed-input do (set (CAR x) (+ (eval (CAR x)) (CDR x))))
    (* forward (- down up))))

;; Like a but up and down now represent aim. When forward is hit, the value of depth is changed as well
(defun part-b (parsed-input)
  (let ((forward 0) (depth 0) (up 0) (down 0))
    (loop for x in parsed-input do
      (set (CAR x) (+ (eval (CAR x)) (CDR x)))
      (if (eq (CAR x) 'forward) (setf depth (+ depth (* (CDR x) (- down up))))))
    (* forward depth)))
