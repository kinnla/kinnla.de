{- ==============================================================
Alp 1 - WS 02/03 (Tutorium: Till Zoppke)
Monika Budde / Emrah Somay 
Uebung 5, Aufg. 2


============================================================== -}

-- a) Liste der Quadratzahlen zwischen 1 und n
--    fuer n <= 0 ist qu n = []; aus m >= 1 folgt 1 <= m*m;
--    statt "m <- [1..n]" waere "m <- [1.. ceiling (sqrt(n))]" schoen, 
--    doch das fuehrte zu einem Fehler ("Instances of (RealFrac Int, 
--    Floating Int) required for definition of qu").

qu :: Int -> [Int]
qu n = [m * m | m <- [1..n], m * m <= n]


-----------------------------------------------
-- b) Liste der Teiler von n

{- Wir benutzen die 1. Def. von "Teiler" ("!!!") aus beispiel_5.lhs, da "Teiler" in der Math. auf den ganzen Zahlen ohne 0 (bzw. alternativ auf den ganzen Zahlen insgesamt) definiert wird; die 2. Def. von "Teiler" mit der Beschraenkung nur auf die natuerlichen Zahlen > 0 ist in der Math. mindestens unueblich (ueber den natuerlichen Zahlen gibt es keine zahlentheoretisch relevante Struktur). Mit dieser Def. von "Teiler" gilt wegen 
    mod (-n) n = 0 = mod (-n) (-n) = mod n (-n) fuer n /= 0: 
Der kleinste Teiler m von n ist (-n), der groesste n, wenn n > 0 ist. 
Fuer n < 0 liegen die Teiler in [n..(-n)]. Fuer n==0 ist die Teilerliste die leere Liste. 

Will man nur die natuerlichen Teiler haben (etwa fuer den Primzahltest wie in beispiel_5.lhs), dann kann man unsere Funktion natTeiler verwenden.
-}

(!!!) :: Int -> Int -> Bool
a !!! b = a /= 0 && b `mod` a == 0

teiler :: Int -> [Int]
teiler n 
   | n == 0 = []
   | n > 0 = [m | m <- [(-n)..n], m !!! n]
   | n < 0 = [m | m <- [n..(-n)], m !!! n]
   
   
natTeiler :: Int -> [Int]
natTeiler n 
   | n >= 0 = [m | m <- [1..n], m !!! n]
   | n < 0 = [m | m <- [1..(-n)], m !!! n]
   
   
-------------------------------------------------
-- c) Liste der Anfangselemente (aus Liste von nichtleeren Listen)
--    Die Aufg. ist unterspezifiziert: Alle Listen in der 
--    Argumentliste muessen vom selben Typ sein (nach Tutorium). 
--    anf ist nur definiert, wenn die Liste der leeren Listen in der
--    Argumentliste selbst leer ist. Umweg ueber length ist noetig, da 
--    "k == []" u.ae. zu einem " Cannot justify constraints in 
--    explicitly typed binding"-Fehler fuehrt. 
--    Ist die Arg.liste eine Liste leerer Listen oder enthaelt 
--    sie Listen unterschiedlichen Typs, so fuehrt dies
--    zu nicht abfangbaren Haskell-Fehlermeldungen.


anf :: [[a]] -> [a]
anf l 
    | length [k| k <- l, length k == 0] == 0 = [head n| n <- l]
    | otherwise = error"Keine leeren Listen in der Arg.liste erlaubt"
 
{-- Typbeschraenkung so nicht umgehbar:    
    type ListenT = [b]        -- -> Typ-Fehler
    anf1 :: [ListenT] -> [b]
-}
    

{-***************************
*** Tests zu den Aufgaben ***
zu a): Liste der Quadratzahlen zwischen 1 und n
       Testdaten: 0, -2  ( -> [] ), 1, 26

Main> qu 0
[]
Main> qu (-2)
[]
Main> qu 1
[1]
Main> qu 26
[1,4,9,16,25]
Main>   
_______________________
zu b) Liste der Teiler von n (mit teiler bzw. natTeiler)
      Testdaten: 0 ( -> [] ), -2, 1, 91, 1693 
Main> teiler 0
[]
Main> teiler (-2)
[-2,-1,1,2]
Main> teiler 1
[-1,1]
Main> teiler 91
[-91,-13,-7,-1,1,7,13,91]
Main> teiler 1693
[-1693,-1,1,1693]
Main> 


Main> natTeiler 0
[]
Main> natTeiler (-2)
[1,2]
Main> natTeiler 1
[1]
Main> natTeiler 91
[1,7,13,91]
Main> natTeiler 1693
[1,1693]
Main>     

______________________
zu c) Liste der Anfangselemente (aus Liste von nichtleeren Listen)
      spezielle Werte: Listen mit teilweise leeren Listen, 
                       Listen aus Listen unterschiedlichen Typs
      Testdaten: [[]], [[],[3]], [[3],[]], [[1],['a']]
                        ( -> Fehlermeldungen )
                 [[1..6], [2..8], [3..9]], [[1],[1]]

Main> anf [[]]
Reading file "C:\Dokumente und Einstellungen\monika\Eigene Dateien\1_Informatik\Theoret_Inform\Alp1_02WS\Uebgszettel\Ue5\Aufg2
_ListenOp.hs":
ERROR - Cannot find "show" function for:
*** Expression : anf [[]]
*** Of type    : [a]

Main> anf [[], [3]]

Program error: Keine leeren Listen in der Arg.liste erlaubt

Main> anf [[3], []]

Program error: Keine leeren Listen in der Arg.liste erlaubt

Main> anf [[1], ['a']]
ERROR - Illegal Haskell 98 class constraint in inferred type
*** Expression : anf [[1],['a']]
*** Type       : Num Char => [Char]

Main> anf [[1..6], [2..8], [3..9]]
[1,2,3]
Main> anf [[1], [1]]
[1,1]
Main>
                  
*****************************-} 