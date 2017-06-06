-- Hinweis zu Aufgabe 2b)
-- 1. zeige: entf (x:xs) = x : entf (xs) für alle Nichtvokale x
-- 2. zeige entf xs = entf (entf xs) per Induktion
--    mit Fallunterscheidung im Induktionsschritt
-- 
-- dass elem x "AEIOUaeiou" für einen Vokal x den Wert 'True' ergibt,
-- braucht man nicht weiter begründen.

-- Hinweis zu Aufgabe 3b)
-- Die Funktion soll das Anfangsstück von xs _ausschließlich_ bis zum
-- ersten Auftreten von x liefern


---------------------- aus der Vorlesung -----------------------
--------- Funktionen mit '2' gibts ohne auch im Prelude, -------
--------------------- aber komplizierter -----------------------

-- entfernt alle Vokale aus einem Text
entf :: String -> String
entf [] = []
entf (x:xs)
  | elem2 x "AEIOUaeiou" = entf xs
  | otherwise           = x : entf xs
  
-- prüft, ob ein String diesen Buchstaben enthält
elem2 :: Char -> String -> Bool
elem2 _ []     = False
elem2 x (y:ys) = x == y || elem2 x ys
  
-- verkehrt eine Liste von hinten nach vorne
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

-- bildet aus einer Liste von Int die Summe
sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs


----------------------- wie im Prelude ------------------------

length2 :: [a] -> Int
length2 []    = 0
length2 (_:l) = 1 + length2 l
