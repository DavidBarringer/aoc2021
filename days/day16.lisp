;; Put the given solutions for the examples here
(setq test-sol-a 11)
(setq test-sol-b 9)

(defstruct packet
  version
  type-ID
  length-type-ID
  (sub-packets nil)
  (value nil))

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((hexes (mapcar 'string (concatenate 'list (remove #\Newline (get-file-string input-file))))))
    (apply 'concatenate 'string (loop for h in hexes collect (format nil "~4,'0B" (parse-integer h :radix 16))))))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (sum-sub-packets (CAR (to-packet parsed-input))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (to-value (CAR (to-packet parsed-input))))

(defun sum-sub-packets (packet)
  (if (null (packet-sub-packets packet)) (packet-version packet)
      (+ (packet-version packet) (loop for sub in (packet-sub-packets packet)
				       sum (sum-sub-packets sub)))))

(defun literal-value (bitstream)
  (apply 'concatenate 'string
	 (loop for end = (string= "0" (subseq bitstream 0 1))
	       for four = (subseq bitstream 1 5)
	       do (setf bitstream (subseq bitstream 5))
	       collect four
	       until end)))

(defun to-value (packet)
  (COND
    ((= 4 (packet-type-ID packet)) (packet-value packet))
    ((= 0 (packet-type-ID packet))
     (loop for sub in (packet-sub-packets packet)
	   sum (to-value sub)))
    ((= 1 (packet-type-ID packet))
     (apply '* (loop for sub in (packet-sub-packets packet)
		     collect (to-value sub))))
    ((= 2 (packet-type-ID packet))
     (apply 'min (loop for sub in (packet-sub-packets packet)
		       collect (to-value sub))))
    ((= 3 (packet-type-ID packet))
     (apply 'max (loop for sub in (packet-sub-packets packet)
		       collect (to-value sub))))
    ((= 5 (packet-type-ID packet))
     (if (> (to-value (CADR (packet-sub-packets packet)))
	    (to-value (CAR (packet-sub-packets packet))))
	 1 0))
    ((= 6 (packet-type-ID packet))
     (if (< (to-value (CADR (packet-sub-packets packet)))
	    (to-value (CAR (packet-sub-packets packet))))
	 1 0))
    ((= 7 (packet-type-ID packet))
     (if (= (to-value (CADR (packet-sub-packets packet)))
	    (to-value (CAR (packet-sub-packets packet))))
	 1 0))))

(defun to-packet (bitstream)
  (let* ((version (parse-integer (subseq bitstream 0 3) :radix 2))
	 (type-ID (parse-integer (subseq bitstream 3 6) :radix 2))
	 (length-type-ID (if (= 4 type-ID) nil (parse-integer (subseq bitstream 6 7) :radix 2))))
    (setf bitstream (if length-type-ID (subseq bitstream 7) (subseq bitstream 6)))
    (COND
      ((null length-type-ID)
       (list (make-packet :version version
			  :type-ID type-ID
			  :length-type-ID length-type-ID
			  :value (parse-integer (literal-value bitstream) :radix 2))
	     (+ 6 (/ (length (literal-value bitstream)) 4) (length (literal-value bitstream)))))
      ((= length-type-ID 0)
       (let* ((l (parse-integer (subseq bitstream 0 15) :radix 2))
	      (len l)
	      (sub-packets nil))
	 (setf bitstream (subseq bitstream 15))
	 (loop until (= l 0)
	       for res = (to-packet bitstream)
	       do  (push (CAR res) sub-packets)
		   (setf l (- l (CADR res)))
		  (setf bitstream (subseq bitstream (CADR res))))
	 (list (make-packet :version version
			    :type-ID type-ID
			    :length-type-ID length-type-ID
			    :sub-packets sub-packets)
	       (+ 7 15 len))))
      ((= length-type-ID 1)
       (let ((l (parse-integer (subseq bitstream 0 11) :radix 2))
	     (len 0)
	     (sub-packets nil))
	 (setf bitstream (subseq bitstream 11))
	 (loop until (= l 0)
	       for res = (to-packet bitstream)
	       do (push (CAR res) sub-packets)
		  (decf l)
		  (incf len (CADR res))
		  (setf bitstream (subseq bitstream (CADR res))))
	 (list (make-packet :version version
			    :type-ID type-ID
			    :length-type-ID length-type-ID
			    :sub-packets sub-packets)
	       (+ 7 11 len)))))))


