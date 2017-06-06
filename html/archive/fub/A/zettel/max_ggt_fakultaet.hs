------------- Programmiersession vom Freitag, dem 1.11.2002 ------------

---------------------------------- MAXIMUM ---------------------------------

-- berechnet das Maximum zweier Zahlen
-- vergleiche Vorlesung
max2 :: (Int, Int) -> Int
max2 (x, y) = if x>y then x else y      -- vergleiche beide Zahlen und gib die größere zurück

-- Variante von max2 mit Currying
max2c :: Int -> Int -> Int              -- vergleiche beide Zahlen und gib die größere zurück
max2c x y = if x>y then x else y

-- berechnet das Maximum dreier Zahlen
max3c :: Int -> Int -> Int -> Int
max3c x y z =                           -- die oben definierte Funktion max2c wird wiederverwendet
 max2c x (max2c y z)                    -- Layout: ohne ein einrückendes Leerzeichen 
                                        --         wirft HUGS einen Syntaxfehler


---------------------------------- GGT ----------------------------------

-- berechnet den Größten gemeinsamen Teiler zweier Zahlen
-- die Funktion ruft sich selbst auf, sie ist rekursiv
ggT :: Int -> Int -> Int
ggT i j = if i<j then ggT j i           -- falls die erste Zahl kleiner als die zweite ist,
                                        -- wiederhole den Aufruf mit vertauschten Zahlen
          
          else if j == 0 then i         -- Abbruchbedingung, Rückgabe des Ergebnisses
          
          else ggT j (i `mod` j)        -- wiederhole den Aufruf mit kleineren Werten,
                                        -- gemäss dem mathematischen Verfahren nach Euklid


--------------------------------- FAKULTÄT --------------------------------

-- berechnet die Fakultät einer ganzen Zahl.
-- Wir definieren die Funktion mit dem Typ 'Integer' (statt 'Int'), 
-- um auch sehr große Zahlen testen zu können.
fakultaet :: Integer -> Integer
fakultaet n = if n == 0 then 1          -- Abbruchbedingung. 
                                        -- Fakultät (0) = 1 nach Definition
              else fakultaet (n-1) * n  -- rekursiver Aufruf


-- Variante von fakultaet mit Wächtern statt if/ else
fakultaet0 :: Integer -> Integer
fakultaet0 n | n == 0    = 1            
             | otherwise = fakultaet0 (n-1) * n


-- Variante von fakultaet mit Accumulatortechnik (kommt in der Vorlesung erst später)
fakultaet1 :: Integer -> Integer                -- Kopf der Funktion
fakultaet1 n | n==0      = 0                    -- Fakultät (0) = 1 nach Definition
             | otherwise = fakultaetAcc n n     -- Aufruf der accumulierenden Unterfunktion

        -- accumulierende Unterfunktion.
        -- n ist ein Zähler, der mit jedem Aufruf um 1 verringert wird
        -- in m, dem Accumulator, wird das Ergebnis Schritt für Schritt gesammelt
        where fakultaetAcc :: Integer -> Integer -> Integer
              fakultaetAcc n m = if n == 1 then m                       -- Abbruchbedingung
                                 else fakultaetAcc (n-1) ((n-1) * m)    -- rekursiver Aufruf


----------------------------- TESTLÄUFE --------------------------------

{-
        Main> :l "D:\\hugs\\max_ggt_fakultaet.hs"
        Reading file "D:\hugs\max_ggt_fakultaet.hs":
        
        Hugs session for:
        C:\PROGRAMME\HUGS98\lib\Prelude.hs
        D:\hugs\max_ggt_fakultaet.hs
        Main> max2 (3,4)
        4
        Main> max2 (-1,-1)
        -1
        Main> max2c 3 9
        9
        Main> max3c 3 4 -1
        ERROR - Illegal Haskell 98 class constraint in inferred type
        *** Expression : max3c 3 4 - 1
        *** Type       : Num (Int -> Int) => Int -> Int
        
        Main> max3c 3 4 (-1)
        4
        Main> ggt 10 15
        ERROR - Undefined variable "ggt"
        Main> ggT 10 15
        5
        Main> ggT 0 0
        0
        Main> ggT (-1) (-1)
        {Interrupted!}
        
        Main> fakultaet 5
        120
        Main> fakultaet0 5
        120
        Main> fakultaet1 5
        120
        Main> fakultaet1 500
        12201368259911100687012387854230469262535743428031928421924135883858453731538819
        97605496447502203281863013616477148203584163378722078177200480785205159329285477
        90757193933060377296085908627042917454788242491272634430567017327076946106280231
        04526442188787894657547771498634943677810376442740338273653974713864778784954384
        89595537537990423241061271326984327745715546309977202781014561081188373709531016
        35632443298702956389662891165897476957208792692887128178007026517450776841071962
        43903943225364226052349458501299185715012487069615681416253590566934238130088562
        49246891564126775654481886506593847951775360894005745238940335798476363944905313
        06232374906644504882466507594673586207463792518420045936969298102226397195259719
        09452178233317569345815085523328207628200234026269078983424517120062077146409794
        56116127629145951237229913340169552363850942885592018727433795173014586357570828
        35578015873543276888868012039988238470215146760544540766353598417443048012893831
        38968816394874696588175045069263653381750554781286400000000000000000000000000000
        00000000000000000000000000000000000000000000000000000000000000000000000000000000
        000000000000000
        Main>          
-}