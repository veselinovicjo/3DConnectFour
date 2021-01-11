(defun alpha-beta (table depth a b figure)
	(let
		((moveslist (all-available-moves table figure))
        (bestvalue '()) (bestmove '()) (state '())
        (tmp '()) (alpha a) (beta b))
		(cond 
            ((or (null moveslist) (= depth 0)))
            ((equal figure 'X) 
                (progn
                    (setf bestvalue -999999)
                    (loop for move in moveslist do
                        (progn )))
            ((equal figure 'O)
                (progn
                    (setf bestvalue 999999)
                    (loop for move in moveslist do
                        (progn ))))))))

(defun computer-move (table figure)
    (let ((move (cadr (alpha-beta table 2 -999999 999999 figure))))
        (next-state table figure)))

;;;;; Drawing functions

; Draws a range of numbers or characters
(defun draw-numbers (range) 
	(if (= range 0) '()
		(progn
			(draw-numbers (- range 1)) 
			(format t "~a " (to-number-or-char (- range 1))))))

; Draw lists recursively
(defun draw-list-rec (li)
	(if (null li) '()
		(progn
			(format t "~a " (car li))
			(draw-list-rec (cdr li)))))

; Draws game state recursively
(defun draw-state-rec (state dimension)
	(if (= dimension 0) '()
		(progn
			(mapcar 
				(lambda (x) 
					(draw-list-rec (car x))) state)
			(format t "~%")
			(draw-state-rec (mapcar (lambda (x) (cdr x)) state) (- dimension 1)))))

; Draws initialization message
(defun init-message ()
	(format t "Unesite dimenziju table (4 - 6): "))

; Draws game state
(defun draw-state (state dimension player)
	(run-shell-command "clear || cls")
	(draw-numbers (* dimension dimension))
	(format t "~%~%")
	(draw-state-rec state dimension)
	(format t "~%")
	(draw-numbers (* dimension dimension))
	(format t "~%~%")
	(format t "Na potezu je IGRAC~a: " player))

;;;;; Utility functions

; Replace element inside list on nth position
(defun replace-element (li index el)
	(if (= index 0) (append (list el) (cdr li)) 
		(append (list (car li)) (replace-element (cdr li) (- index 1) el))))

; Generates either numeric or char values 
(defun to-number-or-char (param)
	(if (< param 10) param (code-char (+ 65 (- param 10)))))

; Checks if move is valid
(defun valid-move (table dimension j k)
	(cond
		((or (< j 0) (> j dimension)) '())
		((or (< k 0) (> k dimension)) '())
		((not (check-if-field-free table j k)) '())
		(t t)))

; Checks if field is free
(defun check-if-field-free (table j k)
	(if (check-available-field-col (mapcar (lambda (x) (nth j x)) (nth k table))) t '()))

; Checks if there is at least one available field in the column
(defun check-available-field-col (li)
	(cond
		((null li) '())
		((equal '- (car li)) t)
		(t (check-available-field-col (cdr li)))))

; Move to coordinates
(defun move-to-coords (move dimension)
	(cond 
		((and (<= (char-code move) (char-code #\9)) (>= (char-code move) (char-code #\0))) 
			(list  (mod  (- (char-code move) (char-code #\0)) dimension) (floor (- (char-code move) (char-code #\0)) dimension)))
		((< (char-code move) (char-code #\a)) 
			(list (mod (+ 10 (- (char-code move) (char-code #\A)))  dimension)  (floor (+ 10 (- (char-code move) (char-code #\A))) dimension)))
		((>= (char-code move) (char-code #\a)) 
			(list (mod (+ 10 (- (char-code move) (char-code #\a)))  dimension)  (floor (+ 10 (- (char-code move) (char-code #\a))) dimension)))
		(t '())))

;;;;; State

; Creates state row
(defun create-row (figure dimension)
	(if (= dimension 0) '()
		(append 
			(create-row figure (- dimension 1)) 
			(list figure))))

; Creates state matrix
(defun create-matrix (figure dimension counter)
	(if (= counter 0) '()
		(append 
			(create-matrix figure dimension (- counter 1)) 
			(list (create-row figure dimension)))))

; Creates state
(defun create-state (figure dimension counter)
	(if (= counter 0) '()
		(append 
			(create-state figure dimension (- counter 1)) 
			(list (create-matrix figure dimension dimension)))))

; Creates initial state
(defun initial-state (dimension)
	(create-state '- dimension dimension))

; Plays move
(defun play-move (table figure move)
	(let ((dimension 0) (coords '())))
	(progn
		(setf dimension (length table))
		(setf coords (move-to-coords move dimension))
		(if (valid-move table dimension (car coords) (cadr coords)) 
			(next-state table figure (car coords) (cadr coords))
			(progn
			(format t "Uneli ste neispravan potez, unesite ponovo. ")
			(play-move table figure (read-move))))))

; Next state function
(defun next-state (table figure j k)
	(let ((i 0)))
	(progn
		(setf i (last-available-field-index 1 (reverse (mapcar (lambda (x) (nth j x)) (nth k table))) (length table)))
		(if (< i 0) table
			(replace-element table k (replace-element (nth k table) i (replace-element (nth i (nth k table)) j figure))))))

; Finds last available field index in the column of the game board
(defun last-available-field-index (initial li dimension)
	 (cond
		((null li) -1)
		((equal '- (car li)) (- dimension initial))
		(t (last-available-field-index (1+ initial) (cdr li) dimension))))

; Determines what figure plays next
(defun next-figure (figure)
	(if (equal figure 'X) 'O 'X))

; Determines who plays next
(defun next-player (player)
	(if (= player 1) 2 1))

; Determines if game has ended
(defun has-game-ended (table) '())

;;;;; Application

; Reads move from standard input
(defun read-move ()
	(car (coerce (read-line) 'list)))

; Game Loop
(defun game-loop (current-state previous-state figure player)	
	(draw-state current-state (length current-state) player)
	(if (not (has-game-ended current-state))
		 	(game-loop (play-move current-state figure (read-move)) current-state (next-figure figure) (next-player player))
			(format t "igra je zavrsena")))

; Main function
(defun main ()
	(init-message)
	(game-loop (initial-state (read)) '() 'X 1))

;;;;; Execution

(main)
