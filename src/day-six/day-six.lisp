(in-package :advent-of-code-2023)

(defparameter *sample-input* (uiop:read-file-lines "day-six-sample.txt"))
(defparameter *actual-input* (uiop:read-file-lines "day-six-actual.txt"))
(defparameter test-input (list "7" "12" "200"))


(defun part-one (input)
  (let ((times (cl-ppcre:all-matches-as-strings "\\d+" (first input)))
	(distances (cl-ppcre:all-matches-as-strings "\\d+" (second input))))
    (reduce #'*  (loop for time in times for distance in distances
		       collect (width-of-limits (limits (parse-integer time) (parse-integer distance)))))))

(defun part-two (input)
  (let ((time (list-of-string-to-string (cl-ppcre:all-matches-as-strings "\\d+" (first input))))
	(distance (list-of-string-to-string (cl-ppcre:all-matches-as-strings "\\d+" (second input)))))
    (format t "~A ~A ~%" time distance)
    (format t "Bounds: ~A" (limits (parse-integer time) (parse-integer distance)))
    (width-of-limits (limits (parse-integer time) (parse-integer distance)))
   ))

(defun part-two-redux (input)
  (let ((time (parse-integer (list-of-string-to-string (cl-ppcre:all-matches-as-strings "\\d+" (first input)))))
	(distance (parse-integer (list-of-string-to-string (cl-ppcre:all-matches-as-strings "\\d+" (second input)))))
	(range 0))
    (loop for i from 1 to time
	  do (let ((remaining-time (- time i)))
	       (if (< distance (* remaining-time i)) (setf range (1+ range)))))
    range))

(defun limits (time distance)
  (declare (type (unsigned-byte 64) time))
  (declare (type (unsigned-byte 64) distance))
  (cons   (round (/ (+ (- 0 time)  (sqrt (- (* time time) (* 4 (+ 1 distance))))) 2))
	  (round  (/ (- (- 0 time)  (sqrt (- (* time time) (* 4 (+ 1 distance))))) 2))))

(defun limits (time distance)
  (cons   (/ (+ time  (sqrt (- (* time time) (* 4 (+ 1 distance))))) 2)
	   (/ (- time  (sqrt (- (* time time) (* 4 (+ 1 distance))))) 2)))

(defun width-of-limits (limits)
  (+ 1 (- (car limits) (cdr limits))))

(defun calculate-distance (time hold-time)
  (* hold-time (- time hold-time)))

(defun list-of-string-to-string (str-list)
  (reduce #'(lambda (x c) (concatenate 'string   x c ))
	  str-list))


;; Answer race 1: 2 3 4 5
;; Answer race 2: 4 5 6 7 8 9 10 11
;; Answer race 3: 11 12 13 14 15 16 17 18 19

(width-of-limits (cons   (/ (+ 50748685  (sqrt  (- (* 50748685 50748685) (* 4 (+ 1 242101716911252) )))) 2)
			 (/ (- 50748685  (sqrt  (- (* 50748685 50748685) (* 4 (+ 1 242101716911252) )))) 2)))
(limits 50748685 242101716911252  )
(limits 71530 940200)

(< 940200 (calculate-distance  71530  14 ))
(< 242101716911252 (calculate-distance 50748685 45418184 ))
(< 10 11)

;; x2 -50748685x + 242101716911252 = 0 
