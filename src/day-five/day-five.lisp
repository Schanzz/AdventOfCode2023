(in-package :advent-of-code-2023)

(defparameter *sample-input* (uiop:read-file-lines "example.txt"))
(defparameter *actual-input* (uiop:read-file-lines "test.txt"))

(defparameter test-range (add-range '("50" "98" "2")))

(defclass mapping ()
  ((name :initarg :name
	 :accessor name)
   (ranges :initarg :ranges
			  :accessor ranges)))
(defclass range ()
  ((start :initarg :start
	  :accessor start)
   (width :initarg :width
	  :accessor width)
   (conversion :initarg :conversion
	       :accessor conversion)))

(defmethod in-range? ((r range) number)
  (with-slots (start width) r
      (and (<= start number) (> (+ start width) number))))

(defun part-one (input)
  (let* ((seeds (mapcar 'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" (first input ))))
	 (mapping (build-mapping (cddr input)))
	 (mapping-names (mapcar #'name mapping)))
    (loop for seed in seeds
	  minimize (first
		    (last (let ((current-seed seed))
			    (loop for map in (reverse mapping)		  
				  collect (get-value-from-ranges (ranges map) current-seed)
				  do (setf current-seed (get-value-from-ranges (ranges map) current-seed)))))))))

(defun part-two (input)
  (let* ((seeds (mapcar 'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" (first input ))))
	 (mapping (build-mapping (cddr input))))
    (loop for (start end) on seeds :by #'cddr
	  minimize (loop for i from start upto (+ start end)
			 minimize (first
				  (last (let ((current-seed i))
					  (loop for map in (reverse mapping)			  
						collect (get-value-from-ranges (ranges map) current-seed)
						do (setf current-seed (get-value-from-ranges (ranges map) current-seed))))))))))

(defun part-two (input)
  (let* ((seeds (mapcar 'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" (first input ))))
	 (mapping (build-mapping (cddr input))))
    
    (mapcar #'(lambda (x) (get-ranges (ranges x))) mapping)))


(defun build-mapping (input)
  "Builds a hash set of mappings with keys as mapping type, IE soil, that then give the destination. Mapping of 
seed to soil for example."
  (let ((hash-set (make-hash-table))
	(mappings '())
	(current-map '())
	(current-mapping ""))
    (loop for line in input
	  if (not (string= "" line))
	    do (if (cl-ppcre:scan "[:]" line)
		   (progn
		     (setf current-mapping (first (cl-ppcre:all-matches-as-strings "[\\w|-]+" line)))
		     (setf current-map (make-instance 'mapping :name current-mapping :ranges '()))
		     (push current-map mappings))	       
		   (push (add-range (cl-ppcre:all-matches-as-strings "\\d+" line)) (ranges current-map))) ;;Build the ranges
	  )
    mappings))

(defun add-range (range)
  (let* ((destination-start (parse-integer (first range)))
	 (source-start (parse-integer (second range)))
	 (count (parse-integer (nth 2 range))))
    (make-instance 'range :start source-start :width count :conversion #'(lambda (x) (+ destination-start (- x source-start))))))

(defun get-value-from-ranges (ranges value)
  (let ((found-value value))
    (loop for r in ranges
	  if (in-range? r value)
	     do (setf found-value (funcall (conversion r) value)))
    found-value))

(defun get-ranges (ranges)
  (mapcar #'(lambda(x) (list (start x) (+ (start x) (width x))
			     (funcall (conversion x) (start x)) (funcall (conversion x) (+ (start x) (width x)))))
	  ranges))
(defun make-seed-ranges (seed count)
  (cons seed (+ seed count)))

