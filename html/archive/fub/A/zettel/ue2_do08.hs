{-	=========================================================
	! 2. Übung zu Algorithmen und Programmierung I		!
	!							!
	! David Kaltschmidt & Benjamin Schröter (do_08)		!
	!							!
	! Tutor: Till Zoppke					!
	========================================================= -}

{-
 	======================================================
	1. Funktion, die für zwei gegebene Rechtecke R1 und R2 
 	entscheidet, ob R1 ganz in R2 enthalten ist.
 	======================================================
-}
enth :: ((Int, Int, Int, Int), (Int, Int, Int, Int)) -> Bool
enth ((x1, y1, h1, b1), (x2, y2, h2, b2)) -- Parameter
	| (b1 < 0) = error "Die Breite des ersten Rechtecks (b1) darf nicht negativ sein!"
	| (b2 < 0) = error "Die Breite des zweiten Rechtecks (b2) darf nicht negativ sein!"
	| (h1 < 0) = error "Die Höhe des ersten Rechtecks (h1) darf nicht negativ sein!"
	| (h2 < 0) = error "Die Höhe des zweiten Rechtecks (h2) darf nicht negativ sein!"
{-
	Höhe und Breite der Rechtecke dürfen nicht negativ sein
-}
	| (x1 < x2) = False 
{-
	Wenn die x-Koordinate der linken untere Ecke des ersten Rechteckes 
	kleiner ist als die x-Koordinate der linken unteren Ecke des 
	zweiten Rechteckes, muss mindestens ein Teil des ersten 
	Rechteckes nicht innerhalb des zweiten Rechteckes liegen, sondern
	links vom zweiten. (False)
-}
	| (y1 < y2) = False
{-
	Wenn die y-Koordinate der linken untere Ecke des ersten Rechteckes 
	kleiner ist als die y-Koordinate der linken unteren Ecke des 
	zweiten Rechteckes, muss mindestens ein Teil des ersten 
	Rechteckes nicht innerhalb des zweiten Rechteckes liegen, sondern
	unterhalb vom zweiten. (False)
-}
	| ((x2 + b2) < (x1 + b1)) = False
{-
	Wenn die x-Koordinate der rechten oberen Ecke (x-Koordinate 
	der linken untere Ecke um die Breite b verschoben) des ersten 
	Rechteckes kleiner ist als die y-Koordinate der rechten oberen Ecke
	(x-Koordinate der linken untere Ecke um die Breite b verschoben) des 
	zweiten Rechteckes, muss mindestens ein Teil des ersten 
	Rechteckes nicht innerhalb des zweiten Rechteckes liegen, sondern
	rechts vom zweiten. (False)
-}
	| ((y2 + h2) < (y1 + h1)) = False
{-
	Wenn die y-Koordinate der rechten oberen Ecke (y-Koordinate 
	der linken untere Ecke um die Höhe h verschoben) des ersten 
	Rechteckes kleiner ist als die y-Koordinate der rechten oberen Ecke
	(y-Koordinate der linken untere Ecke um die Höhe h verschoben) des 
	zweiten Rechteckes, muss mindestens ein Teil des ersten 
	Rechteckes nicht innerhalb des zweiten Rechteckes liegen, sondern
	oberhalb vom zweiten. (False)
-}
	| otherwise = True
{-
	Sonst ist das erste Rechteck im zweiten enthalten (True).
-}

{-
	Programmausgabe:
	****************************************************************** 
	Main> enth ((0,0,1,1),(0,0,2,2))
	True
	Main> enth ((0,0,3,3),(0,0,2,2))
	False
	

	Fehlerausgabe:
	****************************************************************** 
	Main> enth ((2,2,-1,1),(2,2,3,3))

	Program error: Die Höhe des ersten Rechtecks (h1) darf nicht negativ sein!

	Main> enth ((2,2,1,-1),(2,2,3,3))

	Program error: Die Breite des ersten Rechtecks (b1) darf nicht negativ sein!

	Main> enth ((2,2,1,1),(2,2,-3,3))

	Program error: Die Höhe des zweiten Rechtecks (h2) darf nicht negativ sein!

	Main> enth ((2,2,1,1),(2,2,3,-3))

	Program error: Die Breite des zweiten Rechtecks (b2) darf nicht negativ sein!
-}


{- 	======================================================
	2. Funktion (2a), prüft ob ein Jahr ein Schaltjahr ist
 	====================================================== -}
schalt :: Integer -> Bool
schalt (y)
	-- wenn das Jahr durch 400 teilbar ist, ist es ein Schaltjahr
	| (mod y 400 == 0) = True	
	
	-- wenn es durch 100 teilbar ist, ist es kein Schaltjahr
	| (mod y 100 == 0) = False
	
	-- wenn es durch 4 teilbar ist, ist es ein Schaltjahr
	| (mod y 4 == 0) = True
	
	-- ansonsten ist das Jahr kein Schaltjahr
	| otherwise = False


{- 	======================================================
	3. Funktion (2b), ermittelt die Anzahl der Tage seit
	dem 1.1. eines Jahres (für Nicht-Schaltjahre)
 	====================================================== -}
tagesNr :: (Integer, Integer) -> Integer
tagesNr (d, m)
	-- Rekursionsverankerung für m==1 (Januar)
	| (m == 1) = d
	
	-- rekursiver Aufruf der Funktion für m-1
	| (m > 1)  = tageImMonat(m-1) + tagesNr(d, m-1) 


{-	======================================================
	ermittelt die Anzahl der Tage seit
	dem 1.1. eines Jahres und beachtet hierbei auch den
	Sonderfall des Schaltjahres.
 	====================================================== -}
tagesNrSchaltjahr ::(Integer, Integer, Integer) -> Integer
tagesNrSchaltjahr (d, m, y)
	-- wenn es sich um ein Schaltjahr handelt und das gesuchte
	-- Datum nach dem Schalttag (29.2.) liegt, wird zu dem Ergebnis
	-- von tagesNr(d, m) der zusätzliche Tag addiert.
	| ((schalt y) && (m > 2)) = (tagesNr(d, m) +1)
	
	-- ansonsten wird das Ergebnis von tagesNr(d, m) für ein
	-- Nicht-Schaltjahr geliefert.
	| otherwise = tagesNr(d, m)
	

{-	======================================================
	Liefert die Anzahl der Tage in einem Monat.
	Für Februar immer 28!
	Wird von tagesNr verwendet.
 	====================================================== -}
tageImMonat :: (Integer) -> Integer
tageImMonat (m)
	-- 28 Tage für den Februar
	| (m == 2) = 28
	
	-- 30 Tage für April, Juni, September und November
	| (m == 4 || m == 6 || m == 9 || m == 11) = 30
	
	-- 31 Tage für alle anderen Monate
	-- (Januar, März, Mai, Juli, August, Oktober und Dezember)
	| otherwise = 31


{- 	======================================================
	4. Funktion (Aufgabe 3), ermittelt den Wochentag für
	ein beliebiges Datum
 	====================================================== -}
kalender :: (Integer, Integer, Integer) -> Integer
{-	anzahlTageSeit111(d, m, y) liefert die Anzahl der Tage seit
		dem 1.1.1. Der 1.1.1 war ein Montag
	mod anzahlTageSeit111(d, m, y) 7 liefert den Wochentag, 
		allerding als Wert von 0 bis 6 
		mit 0 = Sonntag, 1 = Montag, daher ist die 
		folgende Konstruktion nötig:			-}
kalender (d, m, y) = (mod (anzahlTageSeit111(d, m, y)-1) 7)+1


{-	======================================================
	Ermittelt die Anzahl der Tage seit dem 1.1.1
 	====================================================== -}
anzahlTageSeit111 :: (Integer, Integer, Integer) -> Integer
anzahlTageSeit111 (d, m, y)
	-- Rekursionsverankerung
	-- wenn das Jahr 1 ist kann mittels der 
	-- tagesNrSchaltjahr-Funktion die Anzahl der Tage bis
	-- zum gesuchtem Datum ermittelt werden
	| (y == 1) = tagesNrSchaltjahr (d,m,y)
	
	-- rekursiver Aufruf
	-- ermittelt die Tage im aktuellem Jahr und ruft 
	-- anzahlTageSeit111 erneut für den 31.12. im vorherigem 
	-- Jahr auf
	| (y > 1)  = (anzahlTageSeit111(31,12,y-1) + tagesNrSchaltjahr(d,m,y))

{-	Programmausgabe:
	****************************************************************** 
	Main> schalt(1998)
	False
	Main> schalt(1999)
	False
	Main> schalt(2000)
	True
	Main> schalt(2004)
	True
	Main> schalt(1900)
	False

	Main> tagesNr(1,1)
	1
	Main> tagesNr(10,8)
	222
	Main> tagesNr(28,2)
	59
	Main> tagesNr(31,12)
	365

	Main> tagesNrSchaltjahr(1,3,1999)
	60
	Main> tagesNrSchaltjahr(1,3,2000)
	61
	Main> tagesNrSchaltjahr(15,2,2000)
	46

	Main> kalender(1,1,1)
	1
	Main> kalender(5,11,2002)
	2
	Main> kalender(24,12,2002)
	2
	Main> kalender(9,9,1900)
	7
	Main> kalender(17,3,4200)				-}