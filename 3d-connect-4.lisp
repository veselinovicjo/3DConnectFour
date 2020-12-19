;;;;;;;;;;;;;;;;;;;;;;; INICIJALIZACIJA IGRE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun inicijalizacija-igre ()
;    (let* 
;            (n (unesi-velicinu-table ))
;            (prvi-igrac (unesi-prvog-igraca)) ;ovo ne treba da bude promenljiva, to ce resavamo kasnije :)
;    )
;)


;(defun unesi-velicinu-table ()
;    (format t "Unesi velicinu table (4 ili 6): ")
;    (let*
;       (
;            (velicina (read ))
;        )
;        (cond
;            ((not (numberp velicina)) ((format t "Uneta velicina table mora da bude broj.") (unesi-velicinu-table )))
;            ( (or (< velicina 4) (> velicina 6) (= (mod velicina 2) 1)) ((format t "Velicina table moze da bude 4 ili 6.") (unesi-velicinu-table )))
;            (t velicina)
;        )
;    )
;)

;(defun unesi-prvog-igraca ()
;    (format t "Ako prvi igra racunar, unesi 0, ako prvi igra covek, unesi 1: ")
;   (read )
;)

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
		(draw-numbers (* 4 dimension))
		(format t "~%~%")
		(draw-state-rec state dimension)
		(format t "~%")
		(draw-numbers (* 4 dimension))
		(format t "~%~%")))

;;;;; Utility functions

; Generates either numeric or char values 
(defun to-number-or-char (param)
	(if (< param 10) param (code-char (+ 65 (mod param 10)))))

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
	(create-state '- dimension 4))

;;;;; Application

; Initialize Game
(defun initialize-game (dimension)
	(draw-state (initial-state dimension) dimension))

; Game Loop
; TODO Implement method
(defun game-loop ())

; Main function
(defun main ()
	(progn 
		(format t "Unesite dimenziju table (4 - 6): ")
		(initialize-game (read))))

;;;;; Execution

(main)