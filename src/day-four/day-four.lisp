(in-package :advent-of-code-2023)

(defparameter *test-input* (uiop:read-file-lines "day-four-sample.txt"))
(defparameter *actual-input* (uiop:read-file-lines "day-four-actual.txt "))

(defparameter *test-line* "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") ;;Shoiuld score 8 for part one.

(defclass card-game ()
  ((name :initarg :name
	 :accessor name)
   (score :initarg :score
	  :accessor score)))

(defun score-line (line)
  (let* ((numbers (second (cl-ppcre:split ":" line)))
	 (split-numbers (cl-ppcre:split "[|]" numbers))
	 (entries (cl-ppcre:all-matches-as-strings "\\d+" (first split-numbers)))
	 (answers (cl-ppcre:all-matches-as-strings "\\d+" (second split-numbers)))
	 (score 0))
    (loop for num in entries
	  if (member num answers :test #'string=)
	    do (if (= score 0) (setf score 1) (setf score (* score 2))))
    score))

(defun score-line-part-two (line)
  (let* ((numbers (second (cl-ppcre:split ":" line)))
	 (split-numbers (cl-ppcre:split "[|]" numbers))
	 (entries (cl-ppcre:all-matches-as-strings "\\d+" (first split-numbers)))
	 (answers (cl-ppcre:all-matches-as-strings "\\d+" (second split-numbers)))
	 (score 0))
    (loop for num in entries
	  if (member num answers :test #'string=)
	  sum 1)))

(defun part-one ()
  (loop for card in *actual-input*
	sum (score-line card)))


(defun part-two ()
  (let* ((length-of-input (length *actual-input*))
	 (card-hash (make-hash-table)))
    (loop for i from (-  length-of-input 1) downto 0
	  do (let* ((children (score-line-part-two (nth i *actual-input*)))
		    (game (make-instance 'card-game :name (1+ i) :score 1)))
	       (loop for m below children
		     do (let ((child-game (gethash (+ i m 1) card-hash)))
			  (if child-game (setf (score game) (+ (score child-game) (score game))))))
	       (setf (gethash i card-hash) game)))
    (score-hash card-hash)))

(defun score-hash (hash)
  (loop for key being the hash-keys of hash
	using (hash-value value)
	sum (score value)))
