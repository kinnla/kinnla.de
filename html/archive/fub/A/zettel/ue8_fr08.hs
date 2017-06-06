-- ========================================================== 
--                                                        --
--                      8. Übung ALP I                    --
--                                                        --
--                Mathias Otto und Pau Loong Lee          --
--                                                        --
--                        Till Zoppke                     --
--                                                        -- 
--                           fr_08                        --
-- =========================================================== 
--			1. Aufgabe (a)

vok :: String -> String  -- Signatur
vok x = filter nvok x  -- filter erzeugt eine liste mit elementen
-- die die eigenschaft nvok erfüllen
  where nvok c = not(elem c "aeiouäöüAEIOUÄÖÜ")
  
{-
Main> vok ""
""
(21 reductions, 31 cells)
Main> vok "hfls"
"hfls"
(394 reductions, 588 cells)
Main> vok "hallo welt"
"hll wlt"
(724 reductions, 1046 cells) -}
  
--------------------------------------------------------------
--			      (b)
			      
scal :: [Float] -> [Float] -> Float  -- Signatur
scal x y = sum(zipWith (*) x y) -- zipwith (*) multipliziert das
-- erste element aus beiden listen miteinander. sum addiert alle 
-- ergebnisse. das entspricht der formel ax*bx+ay*by+...
-- ist eine liste länger wird nur bis zur länger der kürzeren 
-- durchgegangen.  dadurch werden falsche eingaben ignoriert!

{-
Main> scal [] []
0
(18 reductions, 53 cells)
Main> scal [4] [3]
12
(27 reductions, 35 cells)
Main> scal [4,2] [3,1]
14
(36 reductions, 44 cells)
Main> scal [4,2,3] [3,1]
14
(36 reductions, 44 cells)   -}

--------------------------------------------------------------
--			      (c)

gleich :: [Int] -> Bool  -- Signatur
gleich x = all (==(head x)) x -- all überprüft alle elemente einer
-- liste nach einer bestimmten eigenschaft, hier ob alle gleich dem 
-- 1. element der liste sind.  

{-
Main> gleich []
True
(23 reductions, 48 cells)
Main> gleich [1]
True
(31 reductions, 51 cells)
Main> gleich [1,2]
False
(36 reductions, 57 cells)
Main> gleich [1,1]
True
(37 reductions, 53 cells)    -}

--------------------------------------------------------------
--			      (d)

suk :: [Int] -> [Int] -- Signatur
suk [] = [] -- rekursionsanker
suk (x:xs) = x:(suk (filter (>x) xs)) -- laut vorgabe wird das 1.
-- element in die liste übernommen. die restliste wird mit filter 
-- neu erstellt. dabei werden alle elemente <= dem angehängten
-- entfernt. dann wird das ganze rekursiv aufgerufen mit der neuen liste

{-
Main> suk [2,-1,4,3,1,4,5,3]
[2,4,5]
(143 reductions, 190 cells)
Main> suk []
[]
(18 reductions, 30 cells) -}

-- ===========================================================
--			2. Aufgabe (a)
			
funkt :: (Float -> Float) -> [Float] -> String -- Signatur
funkt f [] = [] -- rekursionsanker
funkt f (x:xs) = show x ++"\t"++ show (f x) ++ "\n" ++ funkt f xs 
-- show x zeigt das element, dann ein tab und dann show (f x). das ist
-- das ergebnis von f(x).  dann ein zeilenumbruch und der rekursive aufruf

{-
Main> putStr(funkt sin [])

(9 reductions, 15 cells)
Main> putStr(funkt sin [2])
2.0     0.909297

(46 reductions, 163 cells)
Main> putStr(funkt sin [2..4])
2.0     0.909297
3.0     0.14112
4.0     -0.756802

(189 reductions, 419 cells)    -}

--------------------------------------------------------------
--			     (b)
			     
-- signatur
zeichne :: (Float -> Float) -> (Float,Float) -> Float -> (Float,Float) -> String
-- erst wird überprüft ob x1<=x2 ist. das dient als rekursionsanker. außerdem kann so bestimmt
-- werden, wann die y's ausgegeben werden müssen.  falls <= dann wird erst der wert, dann ein tab
-- ausgegeben. 
zeichne f (x1,x2) sw (y1,y2) = if sw > 0 then if x1<=x2 then show x1 ++ "\t" ++ 
-- diese if abfrage prüft ob das ergebnis im zu zeichnenden bereich liegt. falls nicht wird in die 
-- nächste zeile gesprungen, sonst die anzahl der leerzeichen berechnent. dann wird das * geschrieben.
-- nach der if-abfrage kommt der rekursive aufruf mit dem um sw erhöhten x1.
  (if f x1 < y1 || f x1 > y2 then "\n" else concat (replicate ((berechnung f x1 y1)-1) " ") ++ 
  "*" ++ "\n") ++ zeichne f (x1+sw,x2) sw (y1,y2) 
-- die erste if-abfrage war false, dann werden die y-werte ausgegeben.  
  else zeichhelp2 (zeichhelp (y1,y2))
  else error"die schrittweite muß größer 0 sein"
  
  
zeichhelp :: (Float,Float) -> [Float] -- signatur
-- tabelle mit allen y-werten wird erstellt. diese werden um 0,5 erhöht.
zeichhelp (y1,y2) = y1:(if y1+0.5<=y2 then zeichhelp (y1+0.5,y2) else [])

zeichhelp2 :: [Float] -> String -- Signatur
zeichhelp2 [] = [] -- rekursionsanker
-- der erste tab ist sowohl der platz der vorher von den x-werten verwendet wurde,
-- als auch der abstand der einzelnen y-werte voneinander. die vorher erstellt liste
-- wird hier als string ausgegeben
zeichhelp2 (y:ys) = "\t" ++ show y ++ zeichhelp2 ys

berechnung :: (Float -> Float) -> Float -> Float -> Int -- signatur
-- berechnung der leerzeichen.  f(x) ist im regelfall wenn es überhaupt gezeichnet
-- werden muß > y1. das nutze ich hier indem ich zähle, nach wievielen zeichen
-- das ganze <= wird. die erhöhung von y1 ist dabei die y-schrittweite von 0,5 *
-- tabzeichen (also 8).
berechnung f x1 y1 = if (f x1) <= y1 then 0 else 1 + berechnung f x1 (y1+(0.5/8))

{-
Main> putStr(zeichne sin (0.6,5) 0.2 (-1,1))
0.6                              *
0.8                                *
1.0                                  *
1.2                                   *
1.4                                    *
1.6                                    *
1.8                                    *
2.0                                   *
2.2                                 *
2.4                               *
2.6                             *
2.8                          *
3.0                       *
3.2                    *
3.4                *
3.6             *
3.8           *
4.0        *
4.2       *
4.4     *
4.6     *
4.8     *
5.0     *
        -1.0    -0.5    0.0     0.5     1.0
(19932 reductions, 29611 cells)                     

das die punkte am ende wie ein strich runtergehen liegt an der rundung. erhöht man
den wert von x2 auf 15 oder höher sieht man das.  es ist nur jeder 2. bogen davon 
betroffen. -}


-- ============================================================
--			3. Aufgabe 
			
quick :: (a -> a -> Bool) -> [a] -> [a] -- signatur
quick gk [] = [] -- rekursionsanker
-- quicksort direkt aus der vorlesung übernommen.  einzige änderung: funktion wird übergeben und
-- dann als vergleichsfunktion aufgerufen.
quick gk (x:xs) = quick gk [y | y <- xs, gk y x] ++ [x] ++ quick gk [y | y <- xs, not (gk y x)]

{-
Main> quick (<=) [2,-1,4,3,1,4,5,3]
[-1,1,2,3,3,4,4,5]
(426 reductions, 655 cells)
Main> quick (<) [2,-1,4,3,1,4,5,3]
[-1,1,2,3,3,4,4,5]
(337 reductions, 521 cells)
Main> quick (>) [2,-1,4,3,1,4,5,3]
[5,4,4,3,3,2,1,-1]
(341 reductions, 533 cells)
Main> quick (>=) [2,-1,4,3,1,4,5,3]
[5,4,4,3,3,2,1,-1]
(427 reductions, 623 cells) -}

-- ============================================================
--			4. Aufgabe

abl:: (Double->Double)->Double->Double->Double -- signatur
abl f eps x = if eps > 0 then (f(x+eps)-f(x))/eps -- formel aus der aufgabenstellung
       else error"eps muß > 0 sein"

{-
Main> abl sin 5 1
0.679509
(28 reductions, 116 cells)
Main> abl sin 0 1
0.841471
(24 reductions, 40 cells)  -}

-- ============================================================
--			5. Aufgabe
			
(>.>) :: (a->b)->(b->c)->(a->c) -- vorgabe von prof. alt
f>.>g = g.f

isSpace2 :: Char -> Bool -- vordef. funktion aus der prelude um die satzzeichen erweitert
isSpace2 c  =  c == ' '  ||  
               c == '\t' ||
               c == '\n' ||
               c == '\r' ||
               c == '\f' ||
               c == '\v' ||
               c == '\xa0' ||
               c == ','  ||
               c == '.'  ||
               c == '!'  ||
               c == '?'  ||
               c == ':'  ||
               c == ';'

finde:: String -> [String] -- vordef. fkt aus prelude. änderung: statt isSpace wird die
-- erweiterung isSpace2 aufgerufen.  die funktion entfernt die formatierungen, leerzeichen
-- und satzzeichen aus dem string und gibt eine liste mit allen wörtern.
finde s = case dropWhile isSpace2 s of
   "" -> []
   s' -> w : finde s''
     where (w, s'') = break isSpace2 s'
     

sort :: [String] -> [String] -- signatur
sort [] = [] -- rekursionsanker
sort x = [minimum x] ++ sort (sorthelp x) -- sortiert die liste aus finde lexikographisch

sorthelp :: [String] -> [String] -- signatur
-- wird für sort benötigt.  erstellt die restliste die beim rekursiven aufruf von sort
-- verwendet wird.  ist das erste element das minimum dann wird der rest übergeben, sonst
-- wird das element in eine liste geschrieben. dies geschieht solange, bis das minimum 
-- das erste mal gefunden wird. der rest der liste wird dann noch an den vorher gespeicherten
-- anfang angehängt.
sorthelp x = if head x == minimum x then tail x else
  [head x] ++ sorthelp (tail x)
  
  
zaehlen :: [String] -> [(Int,String)] -- signatur
zaehlen [] = [] -- rekursionsanker
-- aufruf der hilfsfunktion zum zählen des ersten wortes. danach rekursiver aufruf für das 
-- nächste wort. dieses ist an der n+1 stelle der liste wobei n die anzahl des ersten wortes
-- ist.  mit drop werden genau diese ersten n elemente aus der liste entfernt.
zaehlen (x:xs) = (zaehlenhelper x (x:xs),x):zaehlen (drop (zaehlenhelper x (x:xs)) (x:xs)) 

zaehlenhelper :: String -> [String] -> Int -- signatur
zaehlenhelper _ [] = 0 -- rekursionsanker
-- da die liste (y:ys) sortiert ist kann ich zählen, wieoft x=y ist mit rekursion.
zaehlenhelper x (y:ys) = if x == y then 1 + (zaehlenhelper x ys) else zaehlenhelper x ys


worthaeufig :: String -> [(Int,String)] -- verknüpfung der einzelnen hilfsfunktionen
worthaeufig =  -- von prof. alt vorgegeben.
      finde>.>
      sort>.>
      zaehlen 	
      
{-
Main> worthaeufig "mensch till, diese ganzen testlaeufe zu machen, macht keinen spaß. aber 
das macht nichts, vielleicht macht sich das mit vielen punkten ja bezahlt."
[(1,"aber"),(1,"bezahlt"),(2,"das"),(1,"diese"),(1,"ganzen"),(1,"ja"),(1,"keinen"),(1,"machen"),
(3,"macht"),(1,"mensch"),(1,"mit"),(1,"nichts"),(1,"punkten"),(1,"sich"),(1,"spa\223"),
(1,"testlaeufe"),(1,"till"),(1,"vielen"),(1,"vielleicht"),(1,"zu")]
(43664 reductions, 73202 cells)  -}