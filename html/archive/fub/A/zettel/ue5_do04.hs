{- ################################################################################
   # Tutor: Till Zoppke           ALP Übungsbogen 5              Abgabe: 28.11.02 # 
   #                                                                              #
   # Annika Imme                Bernadett Smolibocki                 Maria Gensel #
   ################################################################################
-}

-- Aufgabe 3
type Posten = (Int, String, Float)
type Rechnung = [Posten]

-- Die Breite der Rechnung kann variiert werden
breite :: Int
breite = 40

-- Beipielrechnung1
myrechnung1 :: Rechnung
myrechnung1 = [(5, "Königs Pilsener", 2.60), (1, "Pommes mit Mayo", 2.10)]

-- Beipielrechnung2
myrechnung2 :: Rechnung
myrechnung2 = [(1,"O-Saft",1.80), (2,"Pizza Salami",3.20), (6,"Coca-Cola",2.10), 
         (2,"Schweinemedaillons mit Kroketten",7.80), 
         (1,"Schnitzer Wiener Art",5.00), (1,"Kartoffelsuppe",4.6),
         (1,"Putengeschnetzeltes mit Reis",7.10)]

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Haupfunktion; Gibt die Rechnung aus (zusammengesetzt aus den Unterfunktionen)
gibRechnung :: Rechnung -> IO()
gibRechnung re = putStr (gibKopf ++ gibZeilen re ++ gibZwischenteil re ++ gibFuss)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Gibt den Kopfteil zurück
gibKopf :: String
gibKopf = "\n\n+" ++ gibStrichGanz ++ "+\n| RECHNUNG:" ++ gibLeerzeichen (breite+13) ++ 
          "Euro |\n|" ++ gibStrichGanz ++ "|\n"

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Gibt alle Rechnungsposten aus
gibZeilen :: Rechnung -> String
gibZeilen [] = ""
gibZeilen (x:xs) = gibZeile x ++ gibZeilen xs

-- Gibt einen einzelnen Posten aus
gibZeile :: Posten -> String
gibZeile (anz,bez,preis) = "| " ++ (gibLeerzeichenInt anz 3) ++ (show anz) ++
                           gibLeerzeichen 3 ++ bez ++ gibLeerzeichenString bez breite ++
                           gibLeerzeichen 4 ++ gibLeerzeichenFloat preis 6 ++
                           showbsd preis ++ gibLeerzeichen 4 ++
                           gibLeerzeichenFloat (fromInt(anz)*preis) 6 ++
                           showbsd (fromInt(anz)*preis) ++ " |\n"
                           
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Gibt den Teil mit der Auflistung der verschiedenen Summen zurück
gibZwischenteil :: Rechnung -> String
gibZwischenteil re = "|" ++ gibStrichGanz ++ "|\n| " ++ gibLeerzeichen (breite+2) ++
                     "Zwischensumme:   " ++ gibLeerzeichenFloat (zwsumme) 7 ++
                     showbsd (zwsumme) ++ " |\n| " ++ gibLeerzeichen (breite+0) ++
                     "Zzgl. 16% MwSt.:   " ++ 
                     gibLeerzeichenFloat (gessumme-zwsumme) 7 ++  
                     showbsd (gessumme-zwsumme) ++ " |\n|" ++ gibLeerzeichen (breite+1) ++
                     gibStrich 26 ++ " |\n|" ++ gibLeerzeichen (breite+5) ++
                     "Gesamtsumme:   " ++ gibLeerzeichenFloat (gessumme) 7 ++
                     showbsd (gessumme) ++ " |\n|" ++ gibLeerzeichen (breite+20) ++
                     gibGleich 7 ++ " |\n"
                 
   where zwsumme = gibSumme re
         gessumme = (zwsumme*625)/609
                  -- Berechnet die Gesamtsumme inkl. MwSt
         gibSumme :: Rechnung -> Float
            -- Berechnet die Gesamtsumme ohne MwSt
         gibSumme [] = 0
         gibSumme ((anz,bez,preis):xs) = (fromInt(anz)*preis) + gibSumme xs

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Gibt den Fussteil zurück
gibFuss :: String
gibFuss = "|" ++ gibLeerzeichen (breite+28)++ "|\n|"  ++ gibStrichGanz ++  
          "|\n| Vielen Dank fuer Ihren Besuch!" ++ gibLeerzeichen (breite-3) ++ 
          "|\n+" ++ gibStrichGanz ++  "+\n\n"    

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Hilfsfunktionen                           
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Gibt eine bestimmte Anzahl an Leerzeichen zurück
-- Die Anzahl berechnet sich aus der Länge des Parameters (String, Float, Int),
-- abgezogen von der gewünschten Länge (a)

gibLeerzeichenString :: String -> Int -> String
gibLeerzeichenString s a 
   | (length s) > a = error "Zu lange Zeichenkette"
   | otherwise = gibLeerzeichen (a-length(s))
   
gibLeerzeichenFloat :: Float -> Int -> String
gibLeerzeichenFloat f a 
   | (length (showbsd f)) > a = error "Zu lange Zahl"
   | otherwise = gibLeerzeichen (a-length(showbsd f))
   
gibLeerzeichenInt :: Int -> Int -> String
gibLeerzeichenInt i a 
   | (length (show i)) > a = error "Zu lange Zahl"
   | otherwise = gibLeerzeichen (a-length(show i))
   
-- Gibt n Leerzeichen aus
gibLeerzeichen :: Int -> String
gibLeerzeichen 0 = ""
gibLeerzeichen n = ' ' : gibLeerzeichen (n-1)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Gibt eine bestimmte Anzahl an Strichen zurück
gibStrich :: Int -> String
gibStrich 0 = ""
gibStrich x = '-':gibStrich (x-1)

gibStrichGanz :: String
gibStrichGanz = gibStrich (28+breite)

gibGleich :: Int -> String
gibGleich 0 = ""
gibGleich x = '=':gibGleich (x-1)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Verbesserte Variante der show-Funktion, die Floats auf zwei Nachkomma-Stellen 
-- gerundet ausgibt
showbsd :: Float -> String
showbsd f 
  | length (show zweinachkomma) == 3 = show fertigzweinachkomma ++ "0"
     -- Fügt eine 0 an, wenn die zweite Nachkommastelle 0 ist.
  | otherwise = show fertigzweinachkomma
   where nachkomma = f - fromInt(round (f-0.5))
         -- Gibt 0.nachkommastellen an
         zweinachkomma = (fromInt(round(nachkomma*100)))/100
         -- Gibt 0.xx an
         fertigzweinachkomma = fromInt(round(f-0.5)) + zweinachkomma
         -- Gibt die Zahl f gerundet auf zwei Nachkommastellen an      

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Testläufe 
{- 
Main> gibRechnung myrechnung1

+--------------------------------------------------------------------+
| RECHNUNG:                                                     Euro |
|--------------------------------------------------------------------|
|   5   Königs Pilsener                               2.60     13.00 |
|   1   Pommes mit Mayo                               2.10      2.10 |
|--------------------------------------------------------------------|
|                                           Zwischensumme:     15.10 |
|                                         Zzgl. 16% MwSt.:      0.40 |
|                                         -------------------------- |
|                                             Gesamtsumme:     15.50 |
|                                                            ======= |
|                                                                    |
|--------------------------------------------------------------------|
| Vielen Dank fuer Ihren Besuch!                                     |
+--------------------------------------------------------------------+

Main> gibRechnung myrechnung2

+--------------------------------------------------------------------+
| RECHNUNG:                                                     Euro |
|--------------------------------------------------------------------|
|   1   O-Saft                                        1.80      1.80 |
|   2   Pizza Salami                                  3.20      6.40 |
|   6   Coca-Cola                                     2.10     12.60 |
|   2   Schweinemedaillons mit Kroketten              7.80     15.60 |
|   1   Schnitzer Wiener Art                          5.00      5.00 |
|   1   Kartoffelsuppe                                4.60      4.60 |
|   1   Putengeschnetzeltes mit Reis                  7.10      7.10 |
|--------------------------------------------------------------------|
|                                           Zwischensumme:     53.10 |
|                                         Zzgl. 16% MwSt.:      1.40 |
|                                         -------------------------- |
|                                             Gesamtsumme:     54.50 |
|                                                            ======= |
|                                                                    |
|--------------------------------------------------------------------|
| Vielen Dank fuer Ihren Besuch!                                     |
+--------------------------------------------------------------------+

Main> gibRechnung [(6, "Mineralwasser klein", 0.90),(3, "Lasagne", 4.50),
(3, "Filetsteak mit Pommes", 15.60)]

+--------------------------------------------------------------------+
| RECHNUNG:                                                     Euro |
|--------------------------------------------------------------------|
|   6   Mineralwasser klein                           0.90      5.40 |
|   3   Lasagne                                       4.50     13.50 |
|   3   Filetsteak mit Pommes                        15.60     46.80 |
|--------------------------------------------------------------------|
|                                           Zwischensumme:     65.70 |
|                                         Zzgl. 16% MwSt.:      1.73 |
|                                         -------------------------- |
|                                             Gesamtsumme:     67.43 |
|                                                            ======= |
|                                                                    |
|--------------------------------------------------------------------|
| Vielen Dank fuer Ihren Besuch!                                     |
+--------------------------------------------------------------------+
                                         
-}               