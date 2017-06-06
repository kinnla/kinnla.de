--Alp 8.Übung Do 10-12, Till Zoppke
--Tobias Fielitz(3816539), Benjamin Kühn (3789219)

--1a)

entvok :: String -> String
entvok xs = [x | x <- xs, (notElem x "AEIOUaeiouÄÜÖäüö")]

--1b)

skalarp :: Num a => [a] -> [a] -> a
skalarp xs ys =if (length xs) /= (length ys) then error "nicht gleich Dimensional" else sum [ (x*y) | (x,y) <- (zip xs ys)]

--1c)

allegleich :: Eq a=> [a] -> Bool
allegleich xs = xs  == (filter ((head xs) == ) xs)

--1d)

maxima :: Ord a => [a] -> [a]
maxima [] = []
maxima (x:xs) = x:maxima (filter (x <) xs)

--2a)

--Schreibt den ersten Wert der Liste, fügt eine bestimmte Anzahl von Leerzeichen hinzu,
--wendet die Funktion auf das erste Element der Liste an, fügt einen Zeilenumbruch ein,
--und wiederholt das Ganze Rekursiv für den Rest der Liste!
funkber :: (Float -> Float) -> [Float] -> String
funkber f [] = ""
funkber f (a:as) = show a ++ "     " ++ show (f a) ++ "\n" ++ funkber f as

--"Formatiert funkber, so dass es wie gewollt ausgegeben wird!
funktab :: (Float -> Float) -> [Float] -> IO()
funktab f (a:as) = putStr (funkber f (a:as))

--Funktionen(vordefiniert) zum Testen
--Quadratische Funktion x²
bla :: Float -> Float
bla x = x^2

--Gerade x+1
lin :: Float -> Float
lin x = x+1

--Darstellung der SinusKurve
singr = putStr(zeichne sin (0,2*pi) 0.1 (-1,1))

--2b)

--zeichne gibt Funktionswerte an zeichne' weiter, und fügt als weiteren Funktionswert die grösste Stellenanzahl 
--entweder von x1 oder x2 (je nachdem welche gösser ist) ein

zeichne :: (Float -> Float) -> (Float,Float) -> Float -> (Float,Float) -> String
zeichne f (x1,x2) s (y1,y2)
  | x1 >= x2 || s <= 0 || y1 >= y2 = error "falsche Eingabe"
  | otherwise = zeichne' f (x1,x2) s (y1,y2) (max (length(show x1)) (length(show x2)))

zeichne' :: (Float -> Float) -> (Float,Float) -> Float -> (Float , Float) -> Int -> String
zeichne' f (x1,x2) s (y1,y2) l
			--wenn x1 grösser als x2 ist, dann ist das die Abbruchbedingung, und nur die y-Achse wird noch gezeichnet
			|x1 > x2 = (leer (l+2)) ++ (strich (60+(length(show y2)))) ++ "\n" ++ (leer (l+2)) ++ (printy y1 ((y2-y1)/4) 4) ++ show y2
			--wenn der Funktionswert nicht im y Intervall liegt, Zeilen umbruch und rek. Aufruf für die nächsten Zeile
			|(f x1) < y1 || (f x1) > y2 = (showx x1 l) ++ "\n" ++ (zeichne' f (x1+s,x2) s (y1,y2) l )
			--y-achse ist beschränkt auf 60 Zeichen; Die Anzahl der Leerzeichen bis zum * ergibt sich aus
			--dem Verhältnis des Funktionswertes zur Intervallgrösse
			|otherwise = (showx x1 l) ++ (leer (abs(round((60* (((f x1)-y1)/ (y2-y1)))))) ) ++ "*" ++ "\n" ++ (zeichne' f (x1+s,x2) s (y1,y2) l )

--gibt String der festen Länge l aus
showx :: Float -> Int -> String
showx x l = (show x) ++ leer (l - length(show x)) ++ " |"


--x Startwert; y Schrittweite; z Anzahl der Durchläufe
printy :: Float -> Float -> Int -> String
printy x y z
         |z == 0 = ""
         |otherwise = (show x) ++ (leer (15-(length(show x)))) ++ printy (x+y) y (z-1)

--SchönheitsSpielerei
strich x = take x strichzeichen
strichzeichen = "-" ++ strichzeichen

--Anzahl von Leerzeichen
leer x = take x leerzeichen
leerzeichen = " " ++ leerzeichen


--3)

belquick :: (a -> a -> Bool) -> [a] -> [a]
belquick gk [] = []
belquick gk (x:xs) = (belquick gk [i | i <- xs , gk i x]) ++ [x]++ (belquick gk [j |j <- xs, not (gk j x)])

--4)

abl :: (Float -> Float) -> Float -> Float -> Float
abl f eps x = ((f (x+eps)) - f(x))/eps

--5)

haeufigkeit :: String -> [(Int,String)]
haeufigkeit = zählegleicheworte.words

zählegleicheworte :: [String] -> [(Int,String)]
zählegleicheworte [] = []
zählegleicheworte (x:xs) = ((wieoft x (x:xs)) ,x):(zählegleicheworte (loeschallegleich x xs))

wieoft :: String -> [String] -> Int
wieoft [] _ = 0
wieoft _ [] = 0
wieoft y (x:xs)
    | (elem y (x:xs)) = 1 + (wieoft y xs)
    | otherwise	      = 0

loeschallegleich :: String -> [String] -> [String]
loeschallegleich y xs = [n | n <- xs, n /= y]

{- TESTLÄUFE:

--zu 1a:

entvok "Dieser Text enthält keine Vokale mehr"
"Dsr Txt nthlt kn Vkl mhr"

--zu 1b:

Main> skalarp [] []
0
Main> skalarp [1,2,3] []

Program error: nicht gleich Dimensional

Main> skalarp [1,2,3] [4,5,6]
32

--zu 1c:

Main> allegleich ["a","a","b"]
False
Main> allegleich ["a","a","a"]
True
Main> allegleich [1,2,3,4,5,6,7]
False
Main> allegleich [6,6,6,6,6,6,6,6]
True

--zu 1d:

Main> maxima [2,-1,4,3,1,4,5,3]
[2,4,5]

--zu 2a
Main> funktab (^2) [0..5]
0.0     0.0
1.0     1.0
2.0     4.0
3.0     9.0
4.0     16.0
5.0     25.0

Main> funktab (+2) [0..10]
0.0     2.0
1.0     3.0
2.0     4.0
3.0     5.0
4.0     6.0
5.0     7.0
6.0     8.0
7.0     9.0
8.0     10.0
9.0     11.0
10.0     12.0


--zu 2b:
Main> singr
0.0     |                              *
0.1     |                                 *
0.2     |                                    *
0.3     |                                       *
0.4     |                                          *
0.5     |                                            *
0.6     |                                               *
0.7     |                                                 *
0.8     |                                                    *
0.9     |                                                     *
1.0     |                                                       *
1.1     |                                                         *
1.2     |                                                          *
1.3     |                                                           *
1.4     |                                                            *
1.5     |                                                            *
1.6     |                                                            *
1.7     |                                                            *
1.8     |                                                           *
1.9     |                                                          *
2.0     |                                                         *
2.1     |                                                        *
2.2     |                                                      *
2.3     |                                                    *
2.4     |                                                  *
2.5     |                                                *
2.6     |                                             *
2.7     |                                           *
2.8     |                                        *
2.9     |                                     *
3.0     |                                  *
3.1     |                               *
3.2     |                            *
3.3     |                         *
3.4     |                      *
3.5     |                   *
3.6     |                 *
3.7     |              *
3.8     |            *
3.9     |         *
4.0     |       *
4.1     |     *
4.2     |    *
4.3     |   *
4.4     | *
4.5     | *
4.6     |*
4.7     |*
4.8     |*
4.9     | *
5.0     | *
5.1     |  *
5.2     |   *
5.3     |     *
5.4     |       *
5.5     |         *
5.6     |           *
5.7     |             *
5.8     |                *
5.9     |                   *
6.0     |                      *
6.1     |                         *
6.2     |                            *
         ---------------------------------------------------------------
         -1.0           -0.5           0.0            0.5            1.0
         

Main> putStr (zeichne (+2) (0,10) 0.5 (0,8))
0.0  |               *
0.5  |                   *
1.0  |                      *
1.5  |                          *
2.0  |                              *
2.5  |                                  *
3.0  |                                      *
3.5  |                                         *
4.0  |                                             *
4.5  |                                                 *
5.0  |                                                    *
5.5  |                                                        *
6.0  |                                                            *
6.5  |
7.0  |
7.5  |
8.0  |
8.5  |
9.0  |
9.5  |
10.0 |
      ---------------------------------------------------------------
      0.0            2.0            4.0            6.0            8.0 

-}

