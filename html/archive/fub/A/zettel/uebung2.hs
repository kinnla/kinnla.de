--------- Lösung zum zweiten Übungsblatt von Till Zoppke


----------------------- Aufgabe 1 ----------------------

-- prüft, ob das erste Dreieck ganz im zweiten enthalten ist.
enth :: ((Int,Int,Int,Int), (Int,Int,Int,Int)) -> Bool
enth ((x1, y1, h1, b1), (x2, y2, h2, b2))
        | h1<0 || h2<0 || b1<0 || b2<0          -- Test auf richtige Eingabe
        = error "Höhe und Breite müssen >=0 sein"
        | otherwise
        =  x1 >= x2                             -- linker Rand
       && y1 >= y2                              -- unterer Rand
       && x1+b1 <= x2+b2                        -- rechter Rand
       && y1+h1 <= y2+h2                        -- oberer Rand


---------------------- Aufgabe 2a ------------------------

-- prüft, ob ein Jahr Schaltjahr ist
schalt :: Int -> Bool
schalt j = (j `mod` 4) == 0      -- durch vier teilbar
        && (j `mod` 100) /= 0    -- aber nicht durch 100
        || (j `mod` 400) == 0    -- es sei denn durch 400

        
--------------------- Aufgabe 2b -------------------------

-- berechnet die Nummer des Tages in einem Nicht-Schaltjahr
tagesNr :: (Int, Int) -> Int
tagesNr (tag, monat) | tag<1 || tag>31          = error "falscher tag"
                     | monat<1 || monat>12      = error "falscher monat"
                     |otherwise                 = tag + msum (monat-1)  
                     
        -- summiere die Tage der Monate auf.
        where msum :: Int -> Int
              msum 11 = 30 + msum 10
              msum 10 = 31 + msum 9
              msum 9 = 30 + msum 8
              msum 8 = 31 + msum 7
              msum 7 = 31 + msum 6
              msum 6 = 30 + msum 5
              msum 5 = 31 + msum 4
              msum 4 = 30 + msum 3
              msum 3 = 31 + msum 2
              msum 2 = 28 + msum 1
              msum 1 = 31
              msum 0 = 0


------------------------ Aufgabe 3 ----------------------------

--  berechnet den Wochentag zu einem Datum
kalender :: (Int, Int, Int) -> Int
kalender (tag, monat, jahr)
         | jahr == 0    = error "Das Jahr 0 gab es nicht" -- sagt Michael
         
           -- berechne die Anzahl der Tage in diesem Jahr
           -- plus die Anzahl der Tage in Differenz zum Jahr 0
         | otherwise    = (tagesNrSchalt (tag, monat, jahr) + jsum (jahr-1)) `mod` 7 + 1
         
  -- berechnet die Nummer des Tages unter Berücksichtigung,
  -- ob es sich um ein Schaltjahr handelt.
  where tagesNrSchalt :: (Int, Int, Int) -> Int
        tagesNrSchalt (tag, monat, jahr) 
        
          -- falls Schaltjahr und der 29. Februar vorbei, dann ein Tag mehr.
          | schalt jahr && monat > 2 = tagesNr (tag, monat) + 1
          | otherwise                = tagesNr (tag, monat)
         
        -- berechnet die Summe aller Tage in Differenz zum Jahr 0
        -- Falls wir vor Christi Geburt sind, so koennen wir bei -1 aufhoeren,
        -- da in "kalender" beim Aufruf von jsum immer eins abgezogen wird, 
        -- und das Jahr 0 nicht existiert.
        jsum :: Int -> Int
        jsum j | j<(-1) = jsum (j+1) - tageImJahr j
               | j>0 = jsum (j-1) + tageImJahr j
               | otherwise = -1
                     
          -- berechnet Anzahl der Tage im Jahr
          -- in Abhängigkeit vom Schaltjahr
          where tageImJahr :: Int -> Int
                tageImJahr k | schalt k = 366
                             | otherwise = 365
                     