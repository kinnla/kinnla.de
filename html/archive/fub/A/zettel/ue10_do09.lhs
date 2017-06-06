==========================================================================
ALP1 - ÜBUNG 11                                   Tutor: Till Zoppke
Nadine Alexander, Philipp Schmidt
==========================================================================

Aufgabe 1
--------------------------------------------------------------------------
f x y = ([x..y], not x)
--------------------------------------------------------------------------

Signatur: f :: Bool -> Bool -> ([Bool],Bool)

Funktion: gibt eine Liste von Bools aus (das erste 2x fuer x = y, 
          [False,True] für x=False,y=True, [] sonst )

--------------------------------------------------------------------------
foldr (+)
--------------------------------------------------------------------------

Signatur: Num a => a -> [a] -> a

Funktion: summiert eine Liste von Zahlen auf und addiert einen Startwert.



--------------------------------------------------------------------------
merge [] ys         = ys
merge xs []         = ys
merge (x:xs) (y:ys)
    | x<y           = x : merge xs (y:ys)
    | otherwise     = y : merge (x:xs) ys
--------------------------------------------------------------------------

Signatur: merge :: Ord a => [a] -> [a] -> [a]

Funktion: merge mischt zwei Listen ineinander und sortiert währenddessen 
          ein bisschen, indem es immer das kleinere der ersten Elemente
          in die gemeinsamme Liste übernimm, und danach das Größere
          mit dem Nächsten der anderen Liste vergleicht. wenn die beiden 
          Eingabelisten sortiert waren, ist es das Ergebnis auch. Wenn 
          nicht, dann sind mindestens die ersten 
          min (length xs) (length ys) sortiert.



Aufgabe 2
--------------------------------------------------------------------------
typ datum
--------------------------------------------------------------------------

> data Monat = Jan | Feb | Mar | Apr | Mai | Jun 
>            | Jul | Aug | Sept | Okt | Nov | Dez
>      deriving (Eq, Ord, Enum, Show)

> data  Datum a b c = L Int Monat Int | N Int Int Int
>      deriving (Eq, Ord)
>

ok.... formate konvertieren macht spass

> flipForm :: (Datum a b c) -> (Datum e f g)
> flipForm (L y m d) = (N y (1+fromEnum m) d)
> flipForm (N y m d) = (L y (toEnum (m-1)) d)

als Instanz 

> instance Enum (Datum a b c) where
>     fromEnum (N y m d) = absTagesNr y m d
>     fromEnum (L y m d) = (fromEnum.flipForm) (L y m d)
>     toEnum i = N y m d 
>            where (y, m, d) = reverseAbsTagesNr i


> absTagesNr :: Int -> Int -> Int -> Int
> absTagesNr jahr monat tag = (  tag + sj + (jahrSumme (jahr-1))
>                             + (sum (take (monat-1) [31,28,31,30,31,30,31,31,30,31,30]))) 
>            where sj = if ((monat > 2) && (schalt jahr)) then 1 else 0 

> schalt::Int->Bool
> schalt jahr = (mod jahr 4==0) && (
>                 (not(mod jahr 100==0)) ||(mod jahr 400==0) )

> jahrSumme :: Int -> Int
> jahrSumme = (\mj -> sum [ jt j | j <- [1..mj]] )

> jt :: Int -> Int
> jt = (\j -> if (schalt j) then 366 else 365)

> reverseAbsTagesNr :: Int -> (Int, Int, Int)
> reverseAbsTagesNr tnr = (jahr, monat, tag)
>        where ujahr = tnr `div` 366

wichtig erst grob abzuschaetzen.... ansonsten ( ujahr=1 )
(2003,9,23)
(73081801 reductions, 88206261 cells, 9 garbage collections)
statt (509502 reductions, 615014 cells)

>              gjahr tnr j | (tnr - (jahrSumme j)) <= jt j = (j+1)
>                          | otherwise = gjahr tnr (j+1)
>              jahr = (gjahr tnr ujahr)
>              tnvjahr = (jahrSumme (jahr-1))
>              monat = gmonat (tnr - tnvjahr) [31,28,31,30,31,30,31,31,30,31,30]
>              gmonat rt (mt:mtl) | (rt - mt) < 1 = 1
>                                 | otherwise = (gmonat (rt-mt) mtl) +1
>              tag = tnr - (absTagesNr jahr monat 0)



> instance Show (Datum a b c) where
>      show (L y m d) = ((show y)++" - "++(show m)++" - "++(show d))
>      show (N y m d) = ((show y)++"-"++(show m)++"-"++(show d))



als Testlauf gibt es datumDemo, die alles auf einmal benutzt...

> datumDemo = (putStr.concat.(map ((++"\n").show.flipForm))) 
>             [(L 2000 Jan 1)..(N 2023 5 42)]

alp1-uez-2003-01-30.lhs
Main> datumDemo
2000 - Jan - 1
2000 - Jan - 2
2000 - Jan - 3
2000 - Jan - 4
2000 - Jan - 5
2000 - Jan - 6
2000 - Jan - 7
2000 - Jan - 8
2000 - Jan - 9
2000 - Jan - 10
2000 - Jan - 11
2000 - Jan - 12
2000 - Jan - 13
2000 - {Interrupted!}

(8155720 reductions, 9845830 cells, 1 garbage collection)



Aufgabe 3
--------------------------------------------------------------------------
typ Figur
--------------------------------------------------------------------------
das steht alles so in der Aufgabe....

> data Figur a b c = Kreis Mittelpunkt Radius 
>            | Rechteck Ecke Ecke
>            | Dreieck Ecke Ecke Ecke
>            | Quadrat Ecke Seite
>      deriving (Eq, Ord)

> type Mittelpunkt = (Float, Float)
> type Ecke = (Float, Float)
> type Radius = Float
> type Seite = Float


zuerst mal ein paar triviale sachen....

> flaeche :: (Figur a b c) -> Float
> flaeche (Quadrat _ s) = s*s
> flaeche (Kreis _ r) = pi*r*r 

sie Seitenlaengen berechnen sich als Differenz der Koordinaten...

> flaeche (Rechteck (x1,y1) (x2,y2)) = abs((x2-y1)*(y2-y1))  

wir benutzen die Heronsche Formel.....

> flaeche (Dreieck (x1,y1) (x2,y2) (x3,y3)) = sqrt(product (s:(map (s-) e)))
>          where e = [a,b,c]
>                s = 0.5*(sum e)

der gute alte Pytagoras muss für die Seitenlängen herhalten...

>                a = sqrt ((q(x2-x1)) + (q(y2-y1))) 
>                b = sqrt ((q(x2-x3)) + (q(y2-y3))) 
>                c = sqrt ((q(x3-x1)) + (q(y3-y1)))

Das Quadrat kann man effizienter als ^2 schreiben....

>                q = (\a -> a*a)

ich fänd es Sinnvoller Figuren anhand ihrer Fläche zu ordnen
(was auch nicht schwer währe), da die Defaultimplementierung
nur anhand der Reihenfolge der Deklaration, danach lexikographisch 
variable für Variable sortiert, was zu solch einleuchtenden 
Ergebnissen führt:

Main> (Kreis (0,0) 500000000 ) <= (Rechteck (1,1) (2,2))
True
Main> (Kreis (0,0) 500000000 ) <= (Kreis (1,1) 0.00000000005)
True


instance Eq (Figur a b c) where
     x == y = (flaeche x) == (flaeche y)·

instance Ord (Figur a b c) where
     compare x y = compare (flaeche x) (flaeche y) 


testlaeufe:
Main> flaeche (Dreieck (1,5) (23,4) (5,96))
1003.0
(116 reductions, 257 cells)
Main> flaeche (Dreieck (0,1) (1,0) (1,1))
0.5
(111 reductions, 177 cells)
Main> flaeche (Rechteck (0,0) (10,10))
100.0
(36 reductions, 50 cells)
Main> flaeche (Quadrat (0,0) 30 )
900.0
(15 reductions, 24 cells)
Main> flaeche (Kreis (0,0) 5 )
78.5398
(33 reductions, 49 cells)


zum zeichnen hatte ich keine Lust mehr.... auf die punkte verzichte
ich zu gunsten meiner Faulheit gerne ;-)
