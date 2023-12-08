(defparameter *test-input* (uiop:read-file-lines "test-input.txt"))
(defparameter *actual-input* (uiop:read-file-lines "problem.txt"))

(defparameter *sample-2-input* (uiop:read-file-lines "sample-2.txt"))


(defun part-one (input)
  (let* ((instructions (first input))
	 (lookup-table (make-lookup-table (cddr input)))
	 (current-key "AAA")
	 (current-instruction (subseq instructions 0 1))
	 (current-value (gethash current-key lookup-table))
	 (i 0))
    (loop while (and (> 100000 i) (not (string= current-key "ZZZ")))
	  do (setf current-instruction (subseq instructions (mod i (length instructions)) (1+ (mod i (length instructions)))))
	  do (setf current-key (get-next-key current-key lookup-table current-instruction))
	  do (setf i (1+ i))
	  do (format t "Keys:~A Instruction: ~A~%"current-key current-instruction))
    (format t "Final answer: ~A" i)))


(defun part-two (input)
  (let* ((instructions (first input))
	 (lookup-table (make-lookup-table (cddr input)))
	 (current-keys (get-starting-keys lookup-table))     
	 (cycle-times (mapcar #'(lambda (x) (find-cycle-time x lookup-table instructions)) current-keys)))
    (format t "Final answer: ~A~%"  (reduce #'lcm cycle-times) )))

(defun find-cycle-time (key lookup-table instructions)
  (let* ((current-instruction (subseq instructions 0 1))
	 (current-key key)
	 (current-value (gethash current-key lookup-table))
	 (count-of-ending '(0))
	 (i 0))
    (loop while (and (> 100000 i) (not (eql 3 (length count-of-ending))))
	  do (setf current-instruction (subseq instructions (mod i (length instructions)) (1+ (mod i (length instructions)))))
	  do (setf current-key (get-next-key current-key lookup-table current-instruction))
	  do (setf i (1+ i))	
	  if (string= "Z" (subseq current-key 2))
	    do (setf count-of-ending (push i  count-of-ending)))
     (- (first count-of-ending) (second count-of-ending))
    ))

(defun get-next-key (k table inst )
  (let ((possible-next-keys (gethash k table)))
    (cond
      ((string= "R" inst) (cdr possible-next-keys))
      ((string= "L" inst) (car possible-next-keys))
      (t (error "Invalid instruction")))))

(defun make-lookup-table (input)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for c in input
	  do (let* ((key (subseq c 0 3))
		    (val (subseq (cl-ppcre:all-matches-as-strings "\\w+" c) 1))
		    (val-cons-cell (cons (first val) (second val))))
	       (progn 
		 ;; (format t "Setting Key: ~A Values: ~A~%" key val-cons-cell)
		 (setf (gethash key hash) val-cons-cell))))
    hash))

(defun get-starting-keys (lookup-table)
  (let* ((keys (alexandria:hash-table-keys lookup-table)))
    (remove-if-not #'(lambda (x) (string= "A" (subseq x 2))) keys)))

(defun all-keys-end (keys)
  (every #'(lambda (x) (string= "Z" (subseq x 2))) keys))
