{- 
2. Klausur in Alp1 WS 02/03
Gruppe (.) (.)
Musterlösung von Till Zoppke


---------------------------- Aufgabe 1 -----------------------------------

a)
x = 5 : []
Wir brauchen folgende Definitionen:
- (:) :: a -> [a] -> [a]
außerdem wissen wir, dass
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
Das zweite Argument für Plus hat den gleichen Typ wie das erste
und somit ist auch q vom Typ Float.
Der Operator (:) verknüpft ein einzelnes Element mit einer Liste.
Das einzelne Element haben wir soeben als T Float identifiziert.
Damit ist r vom Typ [T Float], und die gesamte rechte Seite hat 
diesen Typ. Also ergibt sich als Signatur für x:
x :: Float -> Float -> [T Float] => [T Float]

d)
x = (.) (.)
Das ist nun wieder so ein Spielchen mit Funktionen höherer Ordnung,
wie wir es so lieb gewonnen haben.
(.) :: (b->c) -> (a->b) -> (a->c)
Diese Funktion ist teiweise appliziert mit sich selber.
Damit wir die Lösung systematisch herausbekommen,
schreiben wir die Signatur von (.) noch einmal hin, 
aber mit anderen Variablen:
(.) :: (e->f) -> ((d->e) -> (d->f))
ok, soweit unser Ansatz. Der -> ist rechtsassoziativ,
deshalb habe ich die Klammern so gesetzt. 
Damit haben wir unsere ersten beiden Argumente für die
Applikation identifiziert und erhalten folgende Gleichungen:
b = (e->f)
c = ((d->e) -> (d->f))
Mit Einsetzen in die obere Gleichung und Weglassen von (e->f) ergibt sich:
(.) (.) :: (a->(e->f)) -> (a->((d->e) -> (d->f)))
und jetzt kann man noch umbenennen und übeflüssige Klammern weglassen:
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

   -- ein Kreis mit Radius 0 ist wie ein Rechteck mit Kantenlängen 0
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

-- stellt fest, ob xs ein Präfix von ys ist.
-- Wir müssen zunächst sicherstellen, dass xs kürzer als ys ist.
-- Sonst gäbe es bei take eine Fehlermeldung.
praef :: Eq a => [a] -> [a] -> Bool
praef xs ys = length xs <= length ys && xs == take (length xs) ys

-- Die Häufigkeit des Zeichens c im String ts
haeuf :: Char -> String -> Int
haeuf c = length . filter (==c)

-- Die kleinste natürliche Zahl x mit f x <= g x
-- Es werden die Zahlen in aufsteigender Reihenfolge ausgewertet,
-- deshalb können wir head schreiben, und die erste passende Zahl
-- wird ausgegeben. Bei minimum würde zuerst die Liste vollständig
-- ausgewertet, und die Funktion liefe ewig.
kl :: Ord a => (Int -> a) -> (Int -> a) -> Int
kl f g = head [x | x <- [0..], f x <= g x]

----------------------------- Aufgabe 4 -------------------------------

data Terbaum a = Blatt a | Knoten a (Terbaum a) (Terbaum a) (Terbaum a)

-- Aufruf von help mit maximum
hoehe :: Terbaum a -> Int
hoehe = help maximum 

-- Rekursion durch den Baum. Die übergebene Funktion erechnet
-- aus den Ergebnissen der rekursiven Aufrufe einen Int Wert
help :: ([Int] -> Int) -> Terbaum a -> Int
help f (Blatt _) = 0
help f (Knoten _ a b c) = 1 + f (map (help f) [a,b,c])

-- wir vergleichen die größte Tiefe mit der kleinsten Tiefe.
-- Wenn beide Werte gleich sind, ist der Baum balanciert.
bal :: Terbaum a -> Bool
bal t = help minimum t == help maximum t

{-
c)
zz: Die Anzahl der Blätter eines balancierten ternären Baumes der Höhe h
ist 3 hoch h.

Beweis:
per Induktion über h

Induktionsanfang: h=0
Ein Baum mit Höhe 0 besteht aus genau einem Knoten.
Damit gilt 1 = 3 hoch 0, und der Induktionsanfang ist gezeigt.

Induktionsvoraussetzung:
Die zu zeigende Aussage gelte für ein h.

Induktionsschritt: Zeige, dass die Aussage auch für h+1 gilt.
Ein balancierter Ternärer Baum der Höhe h+1 besteht aus einer Wurzel
mit drei balancierten ternären Bäumen der Höhe h. Die Summe der Blätter
eines Baumes ist gleich der Summe der Blätter der Unterbäume, also gleich
dem Dreifachen der Blätterzahl eines Baumes der Höhe h.
Hier greift die Induktionsvoraussetzung. Ein balancierter Baum der Höhe h
hat 3 hoch h Blätter, also hat ein balancierter Baum der Höhe h+1 
3* (3 hoch h) Blätter = 3 hoch (h+1) Blätter.
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
-- das ist genau die Länge der Liste aller Knotenbeschriftungen
wieGross :: BinBaum a -> Int
wieGross = length . gibAus

-- vereinige erzeugt aus zwei Binbäumen und einer Beschriftung x
-- einen neuen BinBaum
vereinige :: a -> BinBaum a -> BinBaum a -> BinBaum a
vereinige x b1 b2 = Knot x b1 b2

-- zerlege liefert die beiden Unterbäume eines Knotens.
-- Wenn der Baum leer ist, gibt es keine Unterbäume und
-- wir erzeugen eine Fehlermeldung.
zerlege :: BinBaum a -> (BinBaum a, BinBaum a)
zerlege Leer = error "Leerer Baum hat keine zwei Unterbaeume"
zerlege (Knot _ b1 b2) = (b1,b2)

