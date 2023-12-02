(in-package :advent-of-code-2023)

(defclass game ()
  ((game-id
    :initarg :number
    :accessor game-id)
   (red
    :initform 0
    :accessor red)
   (blue
    :initform 0
    :accessor blue)
   (green
    :initform 0
    :accessor green)))

(defmethod add-color ((single-game game) color value)
  (cond
    ((and (string= "red" color) (> value (red single-game))) (setf (red single-game) value))
    ((and (string= "blue" color) (> value (blue single-game)) (setf (blue single-game) value)))
    ((and (string= "green" color) (> value (green single-game)) (setf (green single-game) value)))))

(defparameter test-game (make-instance 'game :number 12))
(defparameter valid-colors '("red" "green" "blue"))

(defparameter input (uiop:read-file-lines "day-two-actual.txt"))
(defparameter test-line "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")

(defun part-one ()
  (let ((games (parse-file-lines input)))
    (reduce '+ (mapcar 'check-game games))))

(defun part-two ()
  (let ((games (parse-file-lines input)))
    (reduce '+ (mapcar 'game-power games))))

(defun check-game (game)
  (if (and (>= 12 (red game)) (>= 13 (green game)) (>= 14 (blue game)))
      (game-id game)
      0))

(defun game-power (game)
  (* (red game) (blue game) (green game)))

(defun parse-file-lines (lines)
  (loop for line in lines
	collect (parse-line line)))

(defun parse-line (line)
  "Parses a line then returns a game with the max colors per round filled out."
  (let* ((split-line (cl-ppcre:split ":" line))
	 (game-segment (first split-line))
	 (game-number (parse-integer (first (cl-ppcre:all-matches-as-strings "\\d+" game-segment))))
	 (current-game (make-instance 'game :number game-number))
	 (rounds (cl-ppcre:split ";" (second split-line))))
    (loop for r in rounds
	  do (loop for color in valid-colors
		   do (add-color current-game color (color-from-round r color))))
	 
    current-game))

(defun color-from-round (round color)
  (let* ((matches (cl-ppcre:all-matches-as-strings (concatenate 'string "\\d+ " "(" color ")") round))
	 (values (mapcar #'(lambda (v)
			     (parse-integer (first (cl-ppcre:all-matches-as-strings "\\d+" v))))
			 matches)))
    (reduce '+ values)))
