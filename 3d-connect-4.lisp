;;;;;;;;;;;;;;;;;;;;;;; INICIJALIZACIJA IGRE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inicijalizacija-igre ()
    (let* 
            (n (unesi-velicinu-table ))
            (prvi-igrac (unesi-prvog-igraca)) ;ovo ne treba da bude promenljiva, to ce resavamo kasnije :)
    )
)


(defun unesi-velicinu-table ()
    (format t "Unesi velicinu table (4 ili 6): ")
    (let*
        (
            (velicina (read ))
        )
        (cond
            ((not (numberp velicina)) ((format t "Uneta velicina table mora da bude broj.") (unesi-velicinu-table )))
            ( (or (< velicina 4) (> velicina 6) (= (mod velicina 2) 1)) ((format t "Velicina table moze da bude 4 ili 6.") (unesi-velicinu-table )))
            (t velicina)
        )
    )
)

(defun unesi-prvog-igraca ()
    (format t "Ako prvi igra racunar, unesi 0, ako prvi igra covek, unesi 1: ")
    (read )
)

 