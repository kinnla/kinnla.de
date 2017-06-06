------------------------------ 8. Übung zu Alp I ------------------------------

---------------------------------- Aufgabe 1 ----------------------------------

-- a)
-- entfernt alle Vokale aus einem Text
entferneVokale :: String -> String
entferneVokale = filter $ flip notElem "aeiouäöüAEIOUÄÖÜ"
        
-- b)
-- berechnet das Skalarprodukt zweier Vektoren
skalarProdukt :: [Float] -> [Float] -> Float
skalarProdukt xs = sum . zipWith (*) xs
         
-- c)
-- testet, ob alle Elemente einer Liste gleich sind

-- einfache Variante mit guter Laufzeit
alleGleich :: [Int] -> Bool
alleGleich xs = null xs || all (== head xs) (tail xs)

-- komplizierte Variante mit guter Laufzeit
alleGleich2 :: [Int] -> Bool
alleGleich2 xs = null . filter (uncurry (/=)) $ zip xs (tail xs)

-- einfache Variante mit schlechter Laufzeit
alleGleich3 :: [Int] -> Bool
alleGleich3 xs = xs == [] || maximum xs == minimum xs

-- komplizierte Variante mit schlechter Laufzeit
alleGleich4 :: [Int] -> Bool
alleGleich4 xs = xs == [] || xs == replicate (length xs) (head xs)

-- d)
-- berechnet die sukzessiven Maxima einer Int-Liste

-- Variante mit linearer Laufzeit
sukzessivesMaximum :: [Int] -> [Int]
sukzessivesMaximum xs = map snd (filter (uncurry (/=)) (zip (head ys + 1 : ys) ys)) 
                        where ys = scanl1 max xs

-- Variante mit quadratischer Laufzeit
sukzessivesMaximum2 :: [Int] -> [Int]
sukzessivesMaximum2 xs 
  = if xs == [] then [] 
    else head xs : [xs!!i | i <- [1 .. length xs - 1], xs!!i > maximum (take i xs)]

-- Variante mit schrecklicher Laufzeit
sukzessivesMaximum3 :: [Int] -> [Int]
sukzessivesMaximum3 xs = if xs == [] then [] 
                         else [x | x <- [minimum ys .. maximum ys], elem x ys] 
                         where ys = scanl1 max xs

---------------------------------- Aufgabe 2 ----------------------------------

-- a)
-- berechnet eine Wertetabelle
tabelle :: (Float -> Float) -> [Float] -> [(Float,Float)]
tabelle f xs = zip xs (map f xs)

-- Ausgabe einer Wertetabelle auf dem Bildschirm
ausgabeTabelle :: (Float -> Float) -> [Float] -> IO()
ausgabeTabelle f = putStr . unlines . map zeile . tabelle f

  -- erzeugt aus einem Werte-Paar eine Tabellenzeile
  where zeile :: (Float,Float) -> String
        zeile (a,b) = show a ++ "\t" ++ show b

-- b)
-- zeichnet ein Diagramm
-- fehlerhafte Eingaben werden abgefangen
zeichne :: (Float -> Float) -> (Float,Float) -> Float -> (Float,Float) -> String
zeichne f (x1,x2) sw (y1,y2)
        | x1 > x2 = error "x1 > x2"
        | y1 >= y2 = error "y1 >= y2"
        | sw <= 0 = error "sw <= 0"
        | otherwise = ausgabe (tabelle f [x1, x1+sw .. x2]) (y1,y2) zeilenLaenge

  where 
  -- formatiert die Wertetabelle als Diagramm
  ausgabe :: [(Float,Float)] -> (Float,Float) -> Int -> String
  ausgabe l a z = unlines (map (zeile a z) l) ++ "\n" ++ skala a z
       
    where 
    -- die horizontale Skala für die letzte Zeile des Diagramms
    skala :: (Float,Float) -> Int -> String
    skala (y1,y2) z = "\t" ++ concatMap (zahl (z `div` 4)) [y1, y1 + (y2-y1)/4 .. y2]
           
      where 
      -- eine Zahl mit der Zeilenlänge entsprechenden Leerzeichen
      zahl :: Int -> Float -> String
      zahl n x = s ++ replicate (n - length s) ' ' where s = show x
     
    -- eine Zeile des Diagramms
    zeile :: (Float,Float) -> Int -> (Float,Float) -> String
    zeile (y1,y2) n (x,y) = show x ++ "\t" ++ replicate i ' ' ++ s
  
      where 
      i = min n $ toInt $ round $ fromInt n * (y-y1) / (y2-y1)
      s = if y1 <= y && y <= y2 then "*" else ""
          
  -- Zeilenlaenge (exklusive Vertikale Skala)
  zeilenLaenge :: Int
  zeilenLaenge = 60

---------------------------------- Aufgabe 3 ----------------------------------

-- Polymorphe Definition von Quicksort mit übergebenem Vergleichsoperator
quicksort :: (a -> a -> Bool) -> [a] -> [a]
quicksort gk [] = []
quicksort gk (x:xs) = quicksort gk [y | y <- xs, gk y x] 
                      ++ (x : quicksort gk [y | y <- xs, not (gk y x)])
                      
---------------------------------- Aufgabe 4 ----------------------------------

-- Differenzenquotient
abl :: (Double -> Double) -> Double -> Double -> Double
abl f eps x = if eps > 0 then (f (x+eps) - f x) / eps
              else error "epsilon kleiner null"

---------------------------------- Aufgabe 5 ----------------------------------

-- zählt die Vorkommen aller Wörter eines Textes, sortiert nach Anzahl Vorkommen
statistik :: String -> [(Int,String)]
statistik s = if null w then [] 
              else quicksort (>) (zaehle (head w) 1 (tail w))
              where w = sortierteWoerter s

-- zählt Wörter in einer sortierten Liste
zaehle :: String -> Int -> [String] -> [(Int, String)]
zaehle s n [] = [(n,s)]
zaehle s n (x:xs) = if s == x then zaehle s (n+1) xs 
                    else (n,s) : zaehle x 1 xs

-- extrahiert aus einem Text eine sortierte Liste von Wörtern
sortierteWoerter :: String -> [String]
sortierteWoerter s = quicksort (<=) (words $ map ersetze s)

-- ersetzt Sonderzeichen durch Leerzeichen und Grossbuchstaben durch Kleinbuchstaben
ersetze :: Char -> Char
ersetze c = if not $ isAlphaNum c then ' ' 
            else toLower c
