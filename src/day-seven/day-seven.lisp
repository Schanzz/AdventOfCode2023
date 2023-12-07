(in-package :advent-of-code-2023)

(defclass hand ()
  ((bid :initarg :bid :accessor bid)
   (cards :initarg :cards :accessor cards)
   (type-of-hand :initarg :type-of-hand :accessor type-of-hand)))

(defparameter sample-hand-string "32J3K 765")

(defparameter sample-hand (let ((split-hand (cl-ppcre:split " " sample-hand-string)))
			    (make-instance 'hand :cards (first split-hand) :bid (second split-hand))))

(defparameter *input-sample* (uiop:read-file-lines "sample.txt"))
(defparameter *actual-input* (uiop:read-file-lines "actual.txt"))

(defparameter *hand-ranks* (reverse'(:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind :two-pair :pair :high-card)))

(defun part-one (input)
  (let* ((values (loop for m in input
		collect (let ((split-hand (cl-ppcre:split " " m)))
			  (make-instance 'hand
					 :bid (parse-integer (second split-hand))
					 :cards (first split-hand)
					 :type-of-hand (score-hand (first split-hand))))))
	 (sorted (sort values #'hand-compare))
	 (summed 
	   (loop for i from 1 upto (length sorted)
		 sum (* i (bid (nth (- i 1) sorted))))))
    (with-open-file (str "output.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (loop for c in sorted do (format str "Card: ~A Bid: ~A Type:~A ~%" (cards c) (bid c) (type-of-hand c))))
    summed))

(defun score-hand (hand)
  (let* ((letter-hash (create-hash-table hand))
	 (results (alexandria:hash-table-alist letter-hash))
	 (number-of-wild (length (cl-ppcre:all-matches-as-strings "J" hand)))
	 (counts (sort (mapcar #'cdr results) #'> ))
	 (adjusted-counts (if (not (eq 5 number-of-wild)) (setf (nth 0 counts) (+ (nth 0 counts) number-of-wild))))
	 (card-value (mapcar #'car (sort results #'> :key #'cdr)))
	 (type-of-hand (if (= 5 number-of-wild)
			     (hand-type "5")
			     (hand-type  (format nil "~{~A~}" counts)))))
    (progn  (format t "Number of Jokers: ~A, Type of hand ~A counts ~A~%" number-of-wild type-of-hand counts)
	    type-of-hand)))

(defun hand-compare (h1 h2)
  (cond
    ((< (position (type-of-hand h1) *hand-ranks*) (position (type-of-hand h2) *hand-ranks*)) t)
    ((eq (position (type-of-hand h1) *hand-ranks*) (position (type-of-hand h2) *hand-ranks*))
     (list< (mapcar #'char-score (coerce (cards h1) 'list)) (mapcar #'char-score (coerce (cards h2) 'list))))
    (t nil)))


(defun create-hash-table (hand)
  (let ((letter-hash (make-hash-table)))
    (loop for m across hand
	  if (not (char= #\J m))
	  do (if (gethash m letter-hash) (setf (gethash m letter-hash) (1+ (gethash m letter-hash))) (setf (gethash m letter-hash) 1)))
    letter-hash))

(defun char-score (c)
  (cond
    ((char= c #\A) 14)
    ((char= c #\K) 13)
    ((char= c #\Q) 12)
    ((char= c #\J) 1) ;;Changed this to one for part two. 11 for part 1
    ((char= c #\T) 10)
    ((char= c #\9) 9)
    ((char= c #\8) 8)
    ((char= c #\7) 7)
    ((char= c #\6) 6)
    ((char= c #\5) 5)
    ((char= c #\4) 4)
    ((char= c #\3) 3)
    ((char= c #\2) 2)
    (t (error (concatenate 'string "Character not supported: " (string c))))))

(defun hand-type (h)
  (cond
    ((string= "51" h) :five-of-a-kind)
    ((string= "41" h) :four-of-a-kind)
    ((string= "321" h) :full-house)
    ((string= "311" h) :three-of-a-kind)
    ((string= "221" h) :two-pair)
    ((string= "2111" h) :pair)
    (t :high-card)))


(defun list< (l1 l2 &key (test #'<))
   (loop for i in l1
	   for j in l2
	 do (format t "Comparing: ~A to ~A ~%" i j)
	 if (not (eq i j))
	     return (funcall test i j)))

(defparameter *lowest-hand* (make-instance 'hand :bid 900 :cards "AJ394" :type-of-hand :high-card))
(defparameter *second-lowest-hand* (make-instance 'hand  :bid 150 :cards "5JT8K" :type-of-hand :high-card))
