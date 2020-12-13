# 3D Connect Four Game in Common Lisp

Koristi se 3D kocka stranice n (n=4, maksimalno n=6), n je paran broj.

Dva igrača crni i beli (X i O) naizmenično odigravaju po jedan potez postavljajući svoje kuglice na proizvoljan štapić.
Igrač, u jednom potezu, može da postavi samo jednu kuglicu. Kuglica se može postaviti samo na štapić koji nije popunjen.

Kocka je na početku prazna.
Igra se završava kada je cela kocka popunjena.

Pobednik je igrač koji spoji više nizova od 4 kuglice u liniji u bilo kom pravcu.
(horizontalno, vertikalno, dijagonalno, kako u ravni tako i u prostoru)
