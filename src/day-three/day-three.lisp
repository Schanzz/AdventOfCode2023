(in-package :advent-of-code-2023)

(defparameter symbols '("/" "@" "!" "#" "$" "%" "^" "&" "*" "+" "=" "-"))
(defparameter char-symbols (mapcar '#))

(defparameter input (uiop:read-file-lines "day-three-actual.txt"))

(defparameter *engine-array* (make-array (list (length input) (length input))))


(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun build-engine-array (input)
  (setf *engine-array* (list-to-2d-array input)))

(defun parse-array (array)
  "Parses the array by scanning for symbols then finding numbers around the symbol"
  (let ((digits '()))
    (destructuring-bind (n m) (array-dimensions array)
      (loop for i from 0 below n do
	(loop for j from 0 below m
	      do
		 (let ((current-element (aref array i j))
		       (current-neighbors (neighbors i j array)))
		   (if (is-number? current-element)
		       (multiple-value-bind (vals neighs) 
			   (loop while (and (< (1+ j) (array-dimension array 0))  (is-number? (aref array i (min (array-dimension array 0) (1+ j)))))
				   do (setf j (1+ j))
				   collect (aref array i j) into vals
				   collect (neighbors i j array) into neigh
				   finally (return (values (push current-element vals) (push current-neighbors neigh))))
			 (setq digits (append digits (list (cons vals (list neighs)))))))))))
    digits))

(defun part-one ()
  (let*((parsed (parse-array *engine-array*))
	(valid-values 
	  (loop for j in parsed
		if (test-symbol-lists (cdr j))
		  collect (parse-integer (coerce (first j) 'string)))))
    (reduce '+ valid-values)))

(defun part-two ()
  (let* ((gears (find-gears *engine-array*))
	 (parsed (parse-array *engine-array*))
	 (matches 
	   (loop for i in gears
		 collect 
		 (loop for p in parsed
		       collect (let ((val (car p))
				     (symb (cdr p)))
				 (loop for (x y v) on (alexandria:flatten symb) by #'cdddr
				       if (equal i (cons x y))
					 return (parse-integer (coerce val 'string))))))))
    (loop for m in matches
	  if (= 2 (length (remove nil m)))
	    sum (reduce #'*  (remove nil m)))))



(defun find-gears (array)
  (let ((indices '()))
    (destructuring-bind (n m) (array-dimensions array)
      (loop for i from 0 below n do
	(loop for j from 0 below m
	      if (char= (aref array i j) #\* )
		do (push (cons i j) indices))))
    indices))

(defun find-numbers-by-gear (index array)
  (neighbors (car index) (cdr index) array))

(defun test-symbol-lists (symbol-list)
  (let* ((valid? nil))	  
    (loop for s in symbol-list	
	  do (loop for j in s
		   do (print j)
		   if (list-contains-symbol j)
		     do (setf valid? t)))
    valid?))

(defun list-contains-symbol (list)
  (some #'char-is-symbol? list))

(defun char-is-symbol? (char)
  (member (coerce  (list (cdr char)) 'string) symbols :test #'string=))


(defun is-number? (num)
  (cl-ppcre:all-matches "\\d" (string num)))

(defun neighbors-part-one (i j array)
  (list
   (aref array (max 0 (- i 1)) (max 0 (- j 1))) ;;0 0
   (aref array (max 0 (- i 1)) j) ;; 0 1 
   (aref array (max 0 (- i 1)) (min (1- (array-dimension array 0)) (+ j 1))) ;; 0 2
   (aref array i (min (1- (array-dimension array 0)) (max 0 (- j 1)))) ;; 1 0
   (aref array i (min (1- (array-dimension array 0)) (+ j 1))) ;; 1,2
   (aref array (min (1- (array-dimension array 0)) (+ i 1)) (max 0 (- j 1))) ;;2 0
   (aref array (min (1- (array-dimension array 0)) (+ i 1)) j);; 2 1
   (aref array (min (1- (array-dimension array 0)) (+ i 1)) (min (1- (array-dimension array 0)) (+ j 1)))));; 2 2
   
(defun neighbors (i j array)
  (list
   (cons (cons (max 0 (- i 1)) (max 0 (- j 1))) (aref array (max 0 (- i 1)) (max 0 (- j 1))));;0 0
   (cons (cons (max 0 (- i 1)) j) (aref array (max 0 (- i 1)) j) );; 0 1 
   (cons (cons (max 0 (- i 1)) (min (1- (array-dimension array 0)) (+ j 1))) (aref array (max 0 (- i 1)) (min (1- (array-dimension array 0)) (+ j 1))));; 0 2
   (cons (cons i (min (1- (array-dimension array 0)) (max 0 (- j 1)))) (aref array i (min (1- (array-dimension array 0)) (max 0 (- j 1)))) );; 1 0
   (cons (cons i (min (1- (array-dimension array 0)) (+ j 1))) (aref array i (min (1- (array-dimension array 0)) (+ j 1))) );; 1,2
   (cons (cons (min (1- (array-dimension array 0)) (+ i 1)) (max 0 (- j 1))) (aref array (min (1- (array-dimension array 0)) (+ i 1)) (max 0 (- j 1))) );;2 0
   (cons (cons (min (1- (array-dimension array 0)) (+ i 1)) j) (aref array (min (1- (array-dimension array 0)) (+ i 1)) j));; 2 1
   (cons (cons (min (1- (array-dimension array 0)) (+ i 1)) (min (1- (array-dimension array 0)) (+ 1 j))) (aref array (min (1- (array-dimension array 0)) (+ i 1)) (min (1- (array-dimension array 0)) (+ j 1))))));; 2 2

   ;;[0,0] [0,1] [0,2]
   ;;[1,0] [1,1] [1,2]
   ;;[2,0] [2,1] [2,2]
   ;;;
