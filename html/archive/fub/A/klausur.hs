{- 
2. Klausur in Alp1 WS 02/03
Gruppe (.) (.)
Musterl�sung von Till Zoppke


---------------------------- Aufgabe 1 -----------------------------------

a)
x = 5 : []
Wir brauchen folgende Definitionen:
- (:) :: a -> [a] -> [a]
au�erdem wissen wir, dass
- 5 :: Num b => b
- [] :: [c]
Daraus ergibt sich, dass a = b und a = c, und somit insgesamt
x = Num a => [a]

b)
x = (3+)
Dies ist eine teilweise Applikation des Operators (+). 
Demnach erwartet (3+) ein Argument weniger und es ergibt sich:
(3+) :: Num a => a -> a

c)
x p q r = (Plus (p+1.0) q) : r
Plus ist ein zweistelliger Konstruktor.
Das erste Argument ist die Summe aus p und einem Float. 
Damit ist die Summe wieder ein Float und auch p vom Typ Float.
Das zweite Argument f�r Plus hat den gleichen Typ wie das erste
und somit ist auch q vom Typ Float.
Der Operator (:) verkn�pft ein einzelnes Element mit einer Liste.
Das einzelne Element haben wir soeben als T Float identifiziert.
Damit ist r vom Typ [T Float], und die gesamte rechte Seite hat 
diesen Typ. Also ergibt sich als Signatur f�r x:
x :: Float -> Float -> [T Float] => [T Float]

d)
x = (.) (.)
Das ist nun wieder so ein Spielchen mit Funktionen h�herer Ordnung,
wie wir es so lieb gewonnen haben.
(.) :: (b->c) -> (a->b) -> (a->c)
Diese Funktion ist teiweise appliziert mit sich selber.
Damit wir die L�sung systematisch herausbekommen,
schreiben wir die Signatur von (.) noch einmal hin, 
aber mit anderen Variablen:
(.) :: (e->f) -> ((d->e) -> (d->f))
ok, soweit unser Ansatz. Der -> ist rechtsassoziativ,
deshalb habe ich die Klammern so gesetzt. 
Damit haben wir unsere ersten beiden Argumente f�r die
Applikation identifiziert und erhalten folgende Gleichungen:
b = (e->f)
c = ((d->e) -> (d->f))
Mit Einsetzen in die obere Gleichung und Weglassen von (e->f) ergibt sich:
(.) (.) :: (a->(e->f)) -> (a->((d->e) -> (d->f)))
und jetzt kann man noch umbenennen und �befl�ssige Klammern weglassen:
(.) (.) :: (a->b->c) -> a -> (d->b) -> c -> b
-}

----------------------------- Aufgabe 2 ------------------------------


-- Definition des Algebraischen Datentypen Geo
data Geo = Punkt (Float, Float) |
           Kreis (Float, Float) Float |
           Rechteck (Float, Float) Float Float |
           Quadrat (Float, Float) Float

instance Eq Geo where
   -- zwei Rechtecke sind gleich, wenn alle Werte gleich sind.
   Rechteck xy1 h1 b1 == Rechteck xy2 h2 b2 = xy1==xy2 && h1==h2 && b1==b2

   -- zwei Kreise sind gleich, wenn alle Werte gleich sind.
   Kreis xy1 r1       == Kreis xy2 r2       = xy1==xy2 && r1==r2

   -- ein Kreis mit Radius 0 ist wie ein Rechteck mit Kantenl�ngen 0
   Kreis xy 0         == g                  = Rechteck xy 0 0 == g
   g                  == Kreis xy 0         = g == Rechteck xy 0 0

   -- ein Kreis mit Radius >0 ist ungleich einem beliebigen anderen
   -- Datentyp.
   Kreis _ _          == g                  = False
   
   -- nachdem nun alle Kreise abgearbeitet sind, haben wir es nur
   -- noch mit Punkten, Rechtecken und Quadraten zu tun.
   -- diese wandeln wir bei Bedarf in Rechtecke um.
   g1                 == g2                 = toRechteck g1 == toRechteck g2

    -- wandelt einen Punkt, ein Quadrat oder ein Rechteck in ein Rechteck um
    -- ein Kreis braucht nicht gematcht werden, da toRechteck nicht
    -- mit einem Kreis als Argument aufgerufen wird.
    where 
      toRechteck :: Geo -> Geo
      toRechteck (Punkt xy) = Rechteck xy 0 0
      toRechteck (Quadrat xy l) = Rechteck xy l l
      toRechteck (Rechteck xy h b) = Rechteck xy h b


---------------------------- Aufgabe 3 -------------------------------

-- multipliziert jedes Element der Liste mit drei und bildet die Summe
fun :: Num a => [a] -> a
fun = sum . map (*3)

-- stellt fest, ob xs ein Pr�fix von ys ist.
-- Wir m�ssen zun�chst sicherstellen, dass xs k�rzer als ys ist.
-- Sonst g�be es bei take eine Fehlermeldung.
praef :: Eq a => [a] -> [a] -> Bool
praef xs ys = length xs <= length ys && xs == take (length xs) ys

-- Die H�ufigkeit des Zeichens c im String ts
haeuf :: Char -> String -> Int
haeuf c = length . filter (==c)

-- Die kleinste nat�rliche Zahl x mit f x <= g x
-- Es werden die Zahlen in aufsteigender Reihenfolge ausgewertet,
-- deshalb k�nnen wir head schreiben, und die erste passende Zahl
-- wird ausgegeben. Bei minimum w�rde zuerst die Liste vollst�ndig
-- ausgewertet, und die Funktion liefe ewig.
kl :: Ord a => (Int -> a) -> (Int -> a) -> Int
kl f g = head [x | x <- [0..], f x <= g x]

----------------------------- Aufgabe 4 -------------------------------

data Terbaum a = Blatt a | Knoten a (Terbaum a) (Terbaum a) (Terbaum a)

-- Aufruf von help mit maximum
hoehe :: Terbaum a -> Int
hoehe = help maximum 

-- Rekursion durch den Baum. Die �bergebene Funktion erechnet
-- aus den Ergebnissen der rekursiven Aufrufe einen Int Wert
help :: ([Int] -> Int) -> Terbaum a -> Int
help f (Blatt _) = 0
help f (Knoten _ a b c) = 1 + f (map (help f) [a,b,c])

-- wir vergleichen die gr��te Tiefe mit der kleinsten Tiefe.
-- Wenn beide Werte gleich sind, ist der Baum balanciert.
bal :: Terbaum a -> Bool
bal t = help minimum t == help maximum t

{-
c)
zz: Die Anzahl der Bl�tter eines balancierten tern�ren Baumes der H�he h
ist 3 hoch h.

Beweis:
per Induktion �ber h

Induktionsanfang: h=0
Ein Baum mit H�he 0 besteht aus genau einem Knoten.
Damit gilt 1 = 3 hoch 0, und der Induktionsanfang ist gezeigt.

Induktionsvoraussetzung:
Die zu zeigende Aussage gelte f�r ein h.

Induktionsschritt: Zeige, dass die Aussage auch f�r h+1 gilt.
Ein balancierter Tern�rer Baum der H�he h+1 besteht aus einer Wurzel
mit drei balancierten tern�ren B�umen der H�he h. Die Summe der Bl�tter
eines Baumes ist gleich der Summe der Bl�tter der Unterb�ume, also gleich
dem Dreifachen der Bl�tterzahl eines Baumes der H�he h.
Hier greift die Induktionsvoraussetzung. Ein balancierter Baum der H�he h
hat 3 hoch h Bl�tter, also hat ein balancierter Baum der H�he h+1 
3* (3 hoch h) Bl�tter = 3 hoch (h+1) Bl�tter.
Damit haben wir den Satz bewiesen.

----------------------------- Aufgabe 5 ----------------------------------

Wir exportieren in diesem Modul den Datentyp BinBaum und die
zu implementierenden Funktionen.

module BinBaum (BinBaum, machLeer, gibAus, wieGross, vereinige, zerlege) where
-}

data BinBaum a = Leer | Knot a (BinBaum a) (BinBaum a)

-- machLeer liefert einen leeren Baum
machLeer :: BinBaum a
machLeer = Leer

-- gibAus gibt alle Knotenbeschriftungen aus (inorder Traversierung)
gibAus :: BinBaum a -> [a]
gibAus Leer = []
gibAus (Knot a t1 t2) = gibAus t1 ++ [a] ++ gibAus t2

-- wieGross liefert die Anzahl aller Knoten.
-- das ist genau die L�nge der Liste aller Knotenbeschriftungen
wieGross :: BinBaum a -> Int
wieGross = length . gibAus

-- vereinige erzeugt aus zwei Binb�umen und einer Beschriftung x
-- einen neuen BinBaum
vereinige :: a -> BinBaum a -> BinBaum a -> BinBaum a
vereinige x b1 b2 = Knot x b1 b2

-- zerlege liefert die beiden Unterb�ume eines Knotens.
-- Wenn der Baum leer ist, gibt es keine Unterb�ume und
-- wir erzeugen eine Fehlermeldung.
zerlege :: BinBaum a -> (BinBaum a, BinBaum a)
zerlege Leer = error "Leerer Baum hat keine zwei Unterbaeume"
zerlege (Knot _ b1 b2) = (b1,b2)

