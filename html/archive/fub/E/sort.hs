-- Quicksort, die Mutter aller Sortierverfahren in Haskell.
-- Implementierung aus AlpI, WS 02/03
quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (x:xs) = quick [y | y<-xs, y<=x] ++ x : quick [y | y<-xs, y>x]

-- Insertionsort. Implementierung mit Akkumulatortechnik
-- Kopf der Funktion. Aufruf wird an Akkumulatorfunktion weitergeleitet,
-- mit leerem Akkumulator
insertion :: (Ord a) => [a] -> [a]
insertion = insertacc []
  
  -- Akkumulatorfunktion. Erstes Argument = unsortierter Teil der Liste.
  -- Zweites Argument = sortierter Teil der Liste.
  -- Rekursionsanker: unsortierter Teil ist leer, Rückgabe sortierter Teil.
  where insertacc :: (Ord a) => [a] -> [a] -> [a]
        insertacc xs []     = xs
        insertacc xs (y:ys) = insertacc (insert xs y) ys
 
          -- Hilfsfunktion. Fügt das Element k an der richtigen Stelle in die
          -- sortierte Liste (l:ls) ein.
          where insert :: (Ord a) => [a] -> a -> [a]
                insert [] k     = [k]
                insert (l:ls) k | l < k     = l : insert ls k
                                | otherwise = k:l:ls

-- Selectionsort. Rekursive Implementation
selection :: (Ord a) => [a] -> [a]
selection [] = []
selection xs = m : selection (remove m xs)
  where m = minimum xs 

        -- Hilfsfunktion. Entfernt das erste Auftreten von k aus
        -- einer Liste. Vorbedingung: k ist in der Liste enthalten.
        remove :: (Ord a) => a -> [a] -> [a]
        remove k (l:ls) | l == k    = ls
                        | otherwise = l : remove k ls
