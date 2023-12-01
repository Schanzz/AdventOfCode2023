(in-package :advent-of-code-2023)

;;Day-1
(defparameter inputs (uiop:read-file-lines "src/day-one-actual.txt "))
(defparameter test-input (uiop:read-file-lines "src/day-one.txt"))
(defparameter valid-words '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))


(defun process-line (line)
  "Takes a line and spits out the correct number. Ex: 1abc3 => 12, treb7uchet => 77"
  (let* ((digits (cl-ppcre:all-matches-as-strings "\\d" line))
	 (first-val (first digits))
	 (second-val (first (last digits))))
    (values (parse-integer (concatenate 'string first-val second-val)))))

(defun part-one ()
  (loop for item in inputs
	sum (process-line item)))

(defun part-two ()
  (loop for item in inputs
	sum (process-line (transform-words item))))


(defun transform-words (line)
  (let* ((indices (loop for m in valid-words
			 if (find-indices line m)
			   collect (find-indices line m)))
	 (sorted (sort indices 'sort-function))
	 (parsed line))
    (loop for m in sorted
	  do (setf parsed (parse-word parsed (car m))))
    parsed))


(defun parse-word(input word)
  (cl-ppcre:regex-replace-all (concatenate 'string "(" word ")") input (word-replacement word)))

(defun word-replacement (word)
  (cond
    ((string= "one" word) "o1e")
    ((string= "two" word) "t2o")
    ((string= "three" word) "t3e")
    ((string= "four" word) "f4r")
    ((string= "five" word) "f5e")
    ((string= "six" word) "s6x")
    ((string= "seven" word) "s7n")
    ((string= "eight" word) "e8t")
    ((string= "nine" word) "n9e")))

(defun find-indices (input word)
  (let ((match (first (cl-ppcre:all-matches (concatenate 'string "(" word ")") input))))
    (if match
	(cons word match))))

(defun sort-function (a b)
  (< (cdr a) (cdr b)))
