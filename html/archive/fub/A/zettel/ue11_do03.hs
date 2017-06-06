-- ALP1 �bung 11
-- Alexander Blotny, Stefan Kadereit
-- Tutor: Till Zoppke
-- Gruppe:Do3

-- Aufgabe 1
-- a)
f::Bool->Bool->([Bool],Bool)
f x y = ([x..y], not x)
{-
Durch den not-Operator wird klar dass es sich bei x nur um einen Boolschen Datentyp handeln kann. Eine Enumeration kann nur bei gleichen Datentypen erfolgen, somit mu� es sich bei y auch um einen Boolschen Datentyp handeln. Durch Unifikation erh�lt man die Signatur.
Diese Funktion gibt nach Eingabe von zwei Boolschen Werten ein Tupel zur�ck das aus einer Liste von Bool und Bool besteht.
-}
-- b)
{-
foldr (+)::Num a=> a->[a]->a
Die allgemeinste Belegung f�r (+) ist Num a und somit mu� der eingegebene Startwert und die Liste von Num a sein.
Nach Eingabe eines Numerischen Startwertes und einer Liste von Numerischen Werten werden alle Werte addiert (inklusive Startwert) und das Ergebnis ausgegeben.
-}
-- c)
merge [] ys = ys -- Zeile kann jede beliebige Liste haben
merge xs [] = xs -- Zeile kann jede beliebige Liste haben
merge (x:xs) (y:ys)
 |x<y = x:merge xs (y:ys) -- durch den (<)-Operator wird klar das die Listen beide vom Typ Ord a sein m�ssen
              -- da nur gleiche Datentypen vergleichbar sind
 |otherwise = y:merge (x:xs) ys -- diese Zeile k�nnte wieder beliebige Datentypen behandeln
{-
Durch die Einschr�nkung in der 4. Zeile ergibt sich:  merge:: Ord a => [a]->[a]->[a]
Diese Funktion verbindet zwei eingegebene Listen zu einer die nach der Gr��e sortiert ist. (von klein nach gro�)
-}

-- Aufgabe 2
-- a)
-- Monat l��t sich mit deriving zur Instanz von Eq,Ord,Enum,Show machen
data Monat = Januar|Februar|M�rz|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember
    deriving (Eq,Ord,Enum,Show)

-- b)
-- Als Erste wird die erste Variante mit der Verwendung von nur Zahlen als Datumseingaben
-- Als Zweite ist die Eingabe mit Monat als Wort deklariert
data Datum = Erste Tag Monat1 Jahr|Zweite Tag Monat2 Jahr
type Tag = Int
type Monat1 = Int
type Monat2 = Monat
type Jahr = Int

-- Hilfsfunktionen f�r Eq
jan::Monat
jan=Januar -- jan als Variable Januar

dez::Monat
dez=Dezember -- dez als Variable Dezember
 
monatsliste::[Monat]
monatsliste = [jan..dez] -- Liste von Januar bis Dezember

-- In der Funktion gleich wird �berpr�ft ob die Zahl des Monats mit dem Monat �bereinstimmt 
-- indem der Monat aus einer geordneten Liste genommen und verglichen wird
gleich::Int->Monat->Bool
gleich m1 m2 = (monatsliste!!(m1-1))==m2 -- Zahl des Monats bestimmt zugeh�rigen Monatsnamen und vergleicht ihn mit m2

-- Instanz Eq f�r Datum
-- Alle Komponenten werden einzeln auf Gleichheit �berpr�ft
instance Eq Datum where
    Erste t1 m1 j1 == Zweite t2 m2 j2 = (t1==t2) && (gleich m1 m2) && (j1==j2)
    Erste t1 m1 j1 == Erste t2 m2 j2 = (t1==t2) && (m1==m2) && (j1==j2)
    Zweite t1 m1 j1 == Zweite t2 m2 j2 = (t1==t2) && (m1==m2) && (j1==j2)
    Zweite t1 m1 j1 == Erste t2 m2 j2 = (t1==t2) && (gleich m2 m1) && (j1==j2)

-- Hilfsfunktionen f�r Ord
-- Hilfsprogramm zur Errechnung der Tagesanzahl bei einem gegebenen Datum
schalt :: Int -> Bool --Bestimmung ob Jahr Schaltjahr ist
schalt j
 | j < 0 = error "Falsche Eingabe"  -- Programmabbruch bei falscher Eingabe
 | (mod j 4==0) && not(mod j 100==0) || (mod j 400==0) = True  -- Jahr ist Schaltjahr
 | otherwise = False  -- Jahr ist kein Schaltjahr

tn :: Int->Int->Int -> Int --Tageszahl von einem Datum in einem bestimmten Jahr wird berechnet
tn t m j
 |m==1 = t -- Rekursionsende. Ausgabe ist die gesuchte Tagesnummer
 |m==3 && schalt(j) = (tn (t+29) (m-1) j) -- Pr�fung auf Schaltjahr (Februar: 29 Tage)
 |m==3 = (tn (t+28) (m-1) j) -- Sonderfall Februar mit 28 Tagen
 |(mod m 2 ==0 || m == 9 || m == 11) && not(m ==10) && not(m ==12) = (tn (t+31) (m-1) j)
    -- Monate mit 31 Tagen werden addiert
 |otherwise = (tn (t+30) (m-1) j)
    -- Monate mit 30 Tagen werden addiert

-- kal berechnet die Anzahl der Tage bis zum eingegebenen Datum
kal :: Int->Int->Int -> Int
kal t m j = ((j-1)*365+div (j-1) 4-div (j-1) 100+div (j-1) 400+ (tn t m j) )
    -- (Jahr-1)*365 + Anzahl der Schaltjahre + Anzahl der Tage im eingegebenen Jahr

-- Umwandlung von Monat in Int um bei der zweiten Variante die Tagesanzahl
-- zu bestimmen ben�tigt man den Monat als Zahl
umw::Monat->Int
umw m = head (filter (`gleich` m) [1..12]) --mit Hilfe der gleich Funktion wird die Stelle von der monatsliste bestimmt


-- Instanz Ord f�r Datum
instance Ord Datum where
    Erste t1 m1 j1 <= Erste t2 m2 j2 = (kal t1 m1 j1) <= (kal t2 m2 j2)
    Erste t1 m1 j1 <= Zweite t2 m2 j2 = (kal t1 m1 j1) <= (kal t2 (umw m2) j2)
    Zweite t1 m1 j1 <= Erste t2 m2 j2 = (kal t1 (umw m1) j1) <= (kal t2 m2 j2)
    Zweite t1 m1 j1 <= Zweite t2 m2 j2 = (kal t1 (umw m1) j1) <= (kal t2 (umw m2) j2)

-- Hilfsfunktionen f�r Enum
-- rest berechnet die vergangenen Jahre zu einer Tagesanzahl
rest::Int->Int->(Int,Int)
rest r j
 |(r>366) && schalt j = rest (r-366) (j+1)
 |(r>365) = rest (r-365) (j+1)
 |otherwise = (r,j) -- Rekursionsanker

-- tage berechnet das Datum anhand der Resttage indem bis r==1 raufgez�hlt wird
tage::(Int,Int)->Int->Int->Datum
tage (1,j) t m = Zweite t (monatsliste!!(m-1)) j -- Rekursionsanker Rest==1, Datum wird ausgegeben
tage (r,j) t m 
 |(m==4)||(m==6)||(m==9)||(m==11) = t30 (r,j) t m -- Monate mit 30 Tagen
 |(m==2) = februar (r,j) t m -- Sonderfall Februar
 |otherwise = t31 (r,j) t m -- Monate mit 31 Tagen

-- Februar mit Schaltjahr wird betrachtet
februar (r,j) t m
 |(t==28) && not(schalt j) = tage ((r-1),j) 1 (m+1)
 |(t==29) = tage ((r-1),j) 1 (m+1)
 |otherwise = tage ((r-1),j) (t+1) m

-- Funktion bei 30 Tagen im Monat
-- es wird ein Tag weiter gesprungen und falls t==30 ein Monat weiter
t30 (r,j) t m 
 |(t==30) = tage ((r-1),j) 1 (m+1)
 |otherwise = tage ((r-1),j) (t+1) m

-- Funktion bei 31 Tagen im Monat
-- es wird ein Tag weiter gesprungen und falls t==31 ein Monat weiter
t31 (r,j) t m 
 |(t==31) = tage ((r-1),j) 1 (m+1)
 |otherwise = tage ((r-1),j) (t+1) m

-- tagesanzahl berechnet das Datum zu einer gegebenen Tagesanzahl vom 1.1.1
-- dabei werden die Hilfsfunktionen tage und rest verwendet
tagesanzahl r = tage (rest r 1) 1 1 

-- Instanz Enum f�r Datum
-- bei Enum mu� vom Datentyp in Int und umgekehrt umgewandelt werden k�nnen
-- dies wird realisiert indem die Tagesanzahl zu einem gegebenem Datum errechnet wird
-- und nat�rlich zu einer gegebenen Tagesanzahl das Datum errechnet wird
instance Enum Datum where
    fromEnum (Erste t m j) = kal t m j
    fromEnum (Zweite t m j) = kal t (umw m) j
    toEnum = tagesanzahl

-- Instanz Show f�r Datum
-- Ausgabe des Datums wird mit Punkten verbessert
instance Show Datum where
    show (Erste t m j) = show t ++ "." ++ show m ++ "." ++ show j
    show (Zweite t m j) = show t ++ "." ++ show m ++ " " ++ show j

-- Aufgabe 3
-- a)
data Figur = Kreis Mittelpkt Radius|Rechteck Ecke1 Ecke2|Dreieck Pkt1 Pkt2 Pkt3|Quadrat Pkt Laenge
    deriving (Eq,Ord)

type Mittelpkt = (Float,Float)
type Radius = Float
type Ecke1 = (Float,Float)
type Ecke2 = (Float,Float)
type Pkt1 = (Float,Float)
type Pkt2 = (Float,Float)
type Pkt3 = (Float,Float)
type Pkt = (Float,Float)
type Laenge = Float

{-
Mit deriving erreicht man das bei der (<=),(>),(<),(>=) Ordnung (Ord) die Reihenfolge in der Definition betrachtet wird, also egal was man f�r Werte eingibt der Kreis ist in diesem Fall immer kleiner als alle anderen Objekte und das Quadrat ist mit beliebigen Eingaben immer gr��er als die andere Objekte. Vergleicht man wiederum gleiche Objekte miteinander werden die einzelnen Komponenten auf Gleichheit verglichen. Dies erfolgt von links nach rechts. Zum Beispiel ist bei einem Kreis das Tupel vom Mittelpkt. gr��er als bei einem zweiten Kreis, so wird dieser als gr��er eingestuft egal ob das zweite Element der Radius beim Zweiten gr��er ist. Bsp.: Kreis (2,2) 1 > Kreis (1,2) 100 ergibt True.
Bei Eq sind verschiedene Objekte immer ungleich. Nur gleiche Objekte k�nnen gleich sein und dann m�ssen auch die Eingaben �bereinstimmen. z.B Kreis (1,2) 3 == Kreis (1,2) 3 ergibt den Wert True aber Kreis (1,3) 4 == Kreis (1,2) 4 den Wert False
-}

-- b)
flaeche:: Figur -> Float
flaeche (Kreis _ r) = pi * r * r -- Berechnung der Fl�che mit der Formel
flaeche (Rechteck (x1,y1) (x2,y2)) = (abs(y1-y2))*(abs(x1-x2)) -- Berechnung von A mit den Seitenl�ngen
flaeche (Dreieck a b c) = sqrt( s * (s-l1) * (s-l2) * (s-l3) ) -- Formel f�r A bei allg. Dreiecken
    where s = umf a b c -- Umfang/2
          l1 = abst a b -- Seite a
          l2 = abst b c -- Seite b
          l3 = abst a c -- Seite c
flaeche (Quadrat _ l) = l*l -- Fl�cheninhalt ist das Quadrat der Seitenl�nge

-- Hilfsfunktionen f�r Dreiecke
-- Abstand zweier Punkte
abst::(Float,Float)->(Float,Float)->Float
abst (x1,y1) (x2,y2) = sqrt (((x2-x1)*(x2-x1)) + ((y2-y1)*(y2-y1))) -- Abstandsgleichung f�r Punkte im Koordinatensystem
-- H�lfte des Umfangs vom Dreieck
umf::(Float,Float)->(Float,Float)->(Float,Float)->Float
umf a b c = ((abst a b) + (abst a c) + (abst b c))/2 -- Umfang wird berechnet und halbiert

--c)
instance Show Figur where
    show (Kreis m r) = kreis m r
    show (Rechteck e1 e2) = rechteck e1 e2
    show (Quadrat p l) = quadrat p l
-- Hilfsfunktionen f�r Show:
-- 1.Kreis

--Berechnung der Kreiswerte
loesungen :: Float->Float->Float->(Float,Float)
loesungen a b c
 |a==0 && b==0 && c==0 = error "alle reellen Zahlen sind Loesung" 
        -- F�r x alle reellen Zahlen einsetzbar, da sich die Gleichung 0=0 ergibt
 |((a==0) && (b==0)) = error "keine Loesungen"
        -- In der Gleichung entsteht eine falsche Aussage (z.B. 5=0, f�r c=5)
        -- c kann nicht 0 sein, da in dem Fall schon vorher abgebrochen wurde
 |(a==0) = ( runde(-(c)/b),runde(-(c)/b))
        -- F�r a=0 entsteht eine Gerade, die nur einen Schnittpunkt mit der x-Achse hat
 |((b^2)/(4*a^2)-(c/a)<0) = error "keine Loesungen"
        -- In der L�sungsformel entsteht ein negativer Wert unter der Wurzel
 |otherwise = (runde((-(b/(2*a))+sqrt((b^2)/(4*a^2)-(c/a)))),runde((-(b/(2*a))-sqrt((b^2)/(4*a^2)-(c/a)))))
        -- Da alle Spezialf�lle behandelt wurden, kann L�sungsformel angewandt werden

-- Argumente werden bestimmt (die die Kreisgleichung erf�llen)
xwerte::Mittelpkt->Radius->Float->(Float,Float)
xwerte (c,d) r y = loesungen 1 (-2*c) (c^2+((y-d)^2)-(r^2))

-- Y-Werte werden errechnet
ywerte::Mittelpkt->Radius->[Float]
ywerte (c,d) r = (d+r):[runde x |x<-[(d+r)-sw,(d+r)-sw-sw..(d-r)+sw]] ++ [(d-r)]
    where sw = runde(((d+r)-(d-r))/40)

-- rundet auf 2 Nachkommastellen
runde::Float->Float
runde x = fromInt(round(x*100))/100

-- Skala f�r die Argumente
xskala::Float->Float->String
xskala x1 x2 ="->x"++"\t"++show x1++leer(abstand ((x1+x2)/2) (x1,x2)-length(show x1))++show((x1+x2)/2)++luecke++show x2
    where luecke = leer(80-(abstand ((x1+x2)/2) (x1,x2))-length(show x2)-length(show((x1+x2)/2)))

-- Zeile f�r den Kreis um zwischen den Kreis-Punkten * zu erzeugen
makeline::(Float,Float)->(Float,Float)->String
makeline (k1,k2) (x1,x2) 
    | (k1==k2) = leer(abstand k1 (x1,x2)) ++ "*" ++ "\n"
    | (k1<k2) = leer(abstand k1 (x1,x2)) ++ stern ( (abstand k2 (x1,x2)) - (abstand k1 (x1,x2))) ++ "\n"
    | (k1>k2) = leer(abstand k2 (x1,x2)) ++ stern ( (abstand k1 (x1,x2)) - (abstand k2 (x1,x2))) ++ "\n"
    | otherwise = ""

-- Berechnet den n�tigen Abstand vom Stern in einer Skala
abstand :: Float->(Float,Float)->Int
abstand k (x1,x2)
 |k>=x1 && k<=x2    = (round(((x1-k)/(x1-x2))*80))
 |otherwise     = 0 -- Falls Wert au�erhalb der Skala

--Gibt n Leerzeichen aus
leer :: Int -> String
leer n = replicate n ' '

--Gibt n Sterne aus
stern :: Int -> String
stern n = replicate n '*'

-- Die einzelnen Zeilen des Kreises werden in einen String gepackt
zeichne::Mittelpkt->Radius->[Float]->String
zeichne _ _ [] =[]
zeichne m r (y:ys) = show y ++ "\t" ++ makeline (xwerte m r y) ((fst(m)-r),(fst(m)+r)) ++ zeichne m r ys

-- Alle Teilprogramme zusammengef�hrt -> Kreiszeichnung
kreis::Mittelpkt->Radius->String
kreis m r = "y"++"\n"++"^"++"\n"++"|" ++ "\n" ++ zeichne m r (ywerte m r) ++ xskala (runde (fst(m)-r)) (runde (fst(m)+r))

-- 2.Rechteck

-- Y-Werte werden in einer Skala bestimmt
fxwerte::Ecke1->Ecke2->[Float]
fxwerte (x1,y1) (x2,y2) = y2:[runde x |x<-[y2-sw,y2-sw-sw..y1+sw]] ++ [y1]
    where sw = runde((y2-y1)/30)

-- Rechteck-Zeilen erden erzeugt
zeilen::[Float]->String
zeilen [] = []
zeilen (y:ys) = show(y) ++ "\t" ++ stern(80) ++ "\n" ++ zeilen ys

-- Alle Teilprogramme zusammengef�hrt -> Rechteckzeichnung
rechteck::Ecke1->Ecke2->String
rechteck e1 e2 = "y"++"\n"++"^"++"\n"++"|" ++ "\n" ++ zeilen (fxwerte e1 e2) ++xskala (runde(fst e1)) (runde(fst e2))

-- 3.Quadrat

-- Zeilen f�r das Quadrat abgestimmt
zeilenq::[Float]->String
zeilenq [] = []
zeilenq (y:ys) = show(y) ++ "\t" ++ stern(65) ++ "\n" ++ zeilenq ys

-- Skala f�r die Argumente f�r das Quadrat
xskalaq::Float->Float->String
xskalaq x1 x2 = "->x"++"\t"++show x1++leer(abstand ((x1+x2)/2) (x1,x2)-length(show x1))++show((x1+x2)/2)++luecke++show x2
    where luecke = leer(65-(abstand ((x1+x2)/2) (x1,x2))-length(show x2)-length(show((x1+x2)/2)))

-- Alle Teilprogramme zusammengef�hrt -> Quadratzeichnung
quadrat::Pkt->Laenge->String
quadrat p l = "y"++"\n"++"^"++"\n"++"|" ++ "\n" ++ zeilenq (fxwerte p p2) ++xskalaq (fst p) ((fst p)+l)
    where p2 = ((fst(p)+l),(snd(p)+l))
{-

Testl�ufe

Main> [Januar .. M�rz]
[Januar,Februar,M�rz]
Main> Januar < M�rz
True
Main> Dezember < M�rz
False
Main> show(Dezember)
"Dezember"
Main> Januar == Januar
True 

Main> Erste 1 2 2002 == Zweite 1 Februar 2002
True
Main> Erste 1 2 2002 < Zweite 3 Februar 2002
True
Main> Erste 1 12 2002 > Zweite 3 Februar 2003
False
Main> show (Erste 1 12 2002)
"1.12.2002"
Main> show (Zweite 1 Dezember 2002)
"1.Dezember 2002"
Main> [Erste 30 1 2003 .. Erste 14 2 2003]
[30.Januar 2003,31.Januar 2003,1.Februar 2003,2.Februar 2003,3.Februar 2003,4.Februar 2003,
5.Februar 2003,6.Februar 2003,7.Februar 2003,8.Februar 2003,9.Februar 2003,10.Febr
uar 2003,11.Februar 2003,12.Februar 2003,13.Februar 2003,14.Februar 2003] 

Main> flaeche (Kreis (0,0) 5)
78.5398
Main> flaeche (Quadrat (1,3) 4)
16.0
Main> flaeche (Rechteck (1,3) (4,0))
9.0
Main> flaeche (Dreieck (1,3) (4,0) (5,3))
6.0 

Main> putStr(show(Kreis (0,0) 4))
y
^
|
4.0                                             *
3.8                                 ************************
3.6                            **********************************
3.4                        ******************************************
3.2                     ************************************************
3.0                  *****************************************************
2.8                **********************************************************
2.6               ************************************************************
2.4             ****************************************************************
2.2            ******************************************************************
2.0          **********************************************************************
1.8         ************************************************************************
1.6        **************************************************************************
1.4       ****************************************************************************
1.2       ****************************************************************************
1.0      ******************************************************************************
0.8      ******************************************************************************
0.6     ********************************************************************************
0.4     ********************************************************************************
0.2     ********************************************************************************
0.0     ********************************************************************************
-0.2    ********************************************************************************
-0.4    ********************************************************************************
-0.6    ********************************************************************************
-0.8     ******************************************************************************
-1.0     ******************************************************************************
-1.2      ****************************************************************************
-1.4      ****************************************************************************
-1.6       **************************************************************************
-1.8        ************************************************************************
-2.0         **********************************************************************
-2.2           ******************************************************************
-2.4            ****************************************************************
-2.6              ************************************************************
-2.8               **********************************************************
-3.0                 *****************************************************
-3.2                    ************************************************
-3.4                       ******************************************
-3.6                           **********************************
-3.8                                ************************
-4.0                                            *
->x     -4.0                                    0.0                                  4.0   

Main> putStr(show(Quadrat (0,0) 4))
y
^
|
4.0     *****************************************************************
3.87    *****************************************************************
3.74    *****************************************************************
3.61    *****************************************************************
3.48    *****************************************************************
3.35    *****************************************************************
3.22    *****************************************************************
3.09    *****************************************************************
2.96    *****************************************************************
2.83    *****************************************************************
2.7     *****************************************************************
2.57    *****************************************************************
2.44    *****************************************************************
2.31    *****************************************************************
2.18    *****************************************************************
2.05    *****************************************************************
1.92    *****************************************************************
1.79    *****************************************************************
1.66    *****************************************************************
1.53    *****************************************************************
1.4     *****************************************************************
1.27    *****************************************************************
1.14    *****************************************************************
1.01    *****************************************************************
0.88    *****************************************************************
0.75    *****************************************************************
0.62    *****************************************************************
0.49    *****************************************************************
0.36    *****************************************************************
0.23    *****************************************************************
0.1     *****************************************************************
0.0     *****************************************************************
->x     0.0                                     2.0                   4.0  

Main> putStr(show(Rechteck (0,0) (4,6)))
y
^
|
6.0     ********************************************************************************
5.8     ********************************************************************************
5.6     ********************************************************************************
5.4     ********************************************************************************
5.2     ********************************************************************************
5.0     ********************************************************************************
4.8     ********************************************************************************
4.6     ********************************************************************************
4.4     ********************************************************************************
4.2     ********************************************************************************
4.0     ********************************************************************************
3.8     ********************************************************************************
3.6     ********************************************************************************
3.4     ********************************************************************************
3.2     ********************************************************************************
3.0     ********************************************************************************
2.8     ********************************************************************************
2.6     ********************************************************************************
2.4     ********************************************************************************
2.2     ********************************************************************************
2.0     ********************************************************************************
1.8     ********************************************************************************
1.6     ********************************************************************************
1.4     ********************************************************************************
1.2     ********************************************************************************
1.0     ********************************************************************************
0.8     ********************************************************************************
0.6     ********************************************************************************
0.4     ********************************************************************************
0.2     ********************************************************************************
0.0     ********************************************************************************
->x     0.0                                     2.0                                  4.0 

-}
