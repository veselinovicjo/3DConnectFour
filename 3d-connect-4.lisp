
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

; Draws game state
(defun draw-state (state dimension)
	(progn
		(format t "~%~%")
		(draw-numbers (* dimension dimension))
		(format t "~%~%")
		(draw-state-rec state dimension)
		(format t "~%")
		(draw-numbers (* dimension dimension))
		(format t "~%~%")))

;;;;; Utility functions

; Generates either numeric or char values 
(defun to-number-or-char (param)
	(if (< param 10) param (code-char (+ 65 (- param 10)))))

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

;;;;; Application

; Initialize Game

(defun initialize-game (dimension)
	(if (or (< dimension 4) (> dimension 6) (= (mod dimension 2) 1)) 
		(progn
			(format t "Uneli ste neispravne dimenzije. Dimenzije mogu biti 4 ili 6. Unesite ponovo:  ")
			(initialize-game (read)))
	(draw-state (initial-state dimension) dimension)))

(defun first-player (fp)
	(if (equal fp 'r)
		(format t "Racunar igra prvi.")
	(format t "Vi igrate prvi.")))


; Game Loop
; TODO Implement method
(defun game-loop ())

; Main function
(defun main ()
	(progn 
		(format t "Unesite dimenziju table (4 ili 6): ")
		(initialize-game (read))
		(format t "Unesite ko igra prvi. Ukoliko zelite da racunar igra prvi, unesite r, u suprotnom, unesite bilo sta drugo: ")
		(first-player (read))))
	
;;;;; Execution

(main)