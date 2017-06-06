{- ==============================================================
Alp 1 - WS 02/03 (Tutorium: Till Zoppke)
Monika Budde / Emrah Somay 
Uebung 5, Aufg. 3

Wir haben den Entwurf aus beispiel_5.lhs etwas abgewandelt.
Die Rechnungsposten erfuellen gewisse Vorbedingungen (s.u.)
============================================================== -}

-- Datentypen, in denen wir unsere Informationen halten

type Posten = (Int, String, Int)  -- (anzahl, ware, EinzelPreis in Ct)
type Rechnung = [Posten]

--Unsere konkrete Rechnung, damit wir nicht alles neu eingeben muessen
-- Mit realitaetsfremden Test-Posten fuer die Grenzwerte aus der
-- Spezifikation

myRechnung :: Rechnung
myRechnung = [(5, "Koepi", 220), (1, "Pommes", 180), (999, "Superbilliges fast-Geschenk-Essen", 5), (77, "Wahnsinnsteures Luxusessen", 99999)]


-- 190 Luxusessen:  definitiv zu teuer
-- Billigessen: Zeilenlg mind. 62

---------------------------
-- makeline :: Posten -> Int -> String
-- erzeugt aus einem Posten eine Zeile einer bestimmten Laenge.
-- Diese Laenge l ist bei uns beschraenkt auf 
--  3 + 1" " + String-Laenge + 7" zu je " + 6 + 2": " + 9 
-- ^Anzahl      ^warenbezeichnung           ^Einzelpr.  ^Summe Posten
--    4            <= 42               9      <=6          9
--         = String-Laenge + 28 <= l <= 70 (Breite DinA4-S.)
-- Precondition fuer Posten (anzahl, ware, einzelPreis): 
--   0 <= anzahl <= 999; 
--   0 <= Laenge d. Warenbezeichnung <= 70-28 = 42  
--   0 <= einzelPreis <= 99999 (= 999,99 EUR)
--   0 <= anzahl * einzelPreis <= 9999999 (=99.999,99 EUR)
--
-- Fuer die Formatierung benutzen wir Hilfsfunktionen.
-- Die Anzahlen werden rechtsbuendig in den Spalten 1-3 ausgegeben, 
-- die Warenbezeichnungen gefolgt von " zu je ", dem Einzelpreis und 
-- ": " wird dann linksbuendig ausgegeben ab der Spalte 5; bis zur 
-- Spalte vor der Ausgabe der Gesamtsumme wird ggfs mit Punkten 
-- aufgefuellt (wenn mind. 3 Zeichenbreiten zur Verfuegung stehen); 
-- die PostenSumme schliesslich wird wieder rechtsbuendig 
-- ausgegeben in den letzten 9 Spalten.


makeLine :: Posten -> Int -> String
makeLine (anz, ware, einzelPr) lg
   | lg - 28 < 0 = error"Mindestlaenge einer Zeile ist 28!"
   | anz < 0 || anz > 999 = error"Anzahl nicht zwischen 0 und 999!"
   | length ware > lg - 28 = error"Warenbezeichnung zu lang!"
   | einzelPr < 0 = error"Einzelpreis negativ!"
   | einzelPr > 99999 = error"Einzelpreis zu hoch!"
   | anz * einzelPr > 9999999 = error"Anzahl * Einzelpreis zu gross!"
   | otherwise = lz3 anz ++ show anz ++ " " ++ ware ++ " zu je " ++ (ct2eur einzelPr) ++ ": " ++ pkt ware einzelPr lg ++ lz9 (sumP(anz, ware, einzelPr)) ++ ct2eur (sumP (anz, ware, einzelPr)) ++ "\n"

-- Leerzeichen vor anz und Gesamtsumme: 
--     3 - Anzahl der Dez.stellen von anz (anz = n)
--     9 - Anzahl der Zeichenstellen von Gesamtsumme (Ges.summe = n)

lz3 :: Int -> String
lz3 n = replicate (3 -(length (show n))) ' '

lz9 :: Int -> String  -- Dez.komma bzw. Dez.komma und Pkt einzufuegen
lz9 n 
    | length (show n) <= 5 = replicate (9 -(length (show n)) - 1) ' ' 
    | otherwise = replicate (9 -(length (show n)) - 2) ' '


-- Punkte nach Ware u. Einzelpreis (= vor Gesamtsumme), abh. von der 
-- insgesamt zur Verfugung stehenden Zeilenlaenge lg:
-- lg abzuegl.: 9 (f. PostenSumme), 9 (f. "zu je ..."), 4 (f. Anz.),
--              Laenge der Warenbezeichnung, Laenge des Einzelpreises
-- Ist der zur Verfuegung stehende Platz weniger als 3 Zeichen breit, 
-- so sollen statt der Punkte Leerzeichen ausgegeben werden. 
-- Wir berechnen mit platz den zur Verfuegung stehenden Leerraum.
-- Beim Platzbedarf fuer den Einzelpreis muss das Dezimalkomma mit 
-- beruecksichtigt werden.

pkt :: String -> Int -> Int -> String
pkt ware pr lg 
   | platz ware pr lg < 2 = replicate (platz ware pr lg) ' '
   | platz ware pr lg >= 2 = replicate (platz ware pr lg) '.'
   | otherwise = error "Rechenfehler bei der Platzberechnung !" 

platz :: String -> Int -> Int -> Int
platz ware pr lg = lg - 22 - length ware - length (ct2eur pr) - 1


-- Cent in Euro-Angabe umformen, groesste relevante Zahl: 999.999,99
-- Abschnittsweise zerlegen und "." bzw. "," einfuegen
-- zu beachten: Pfennigangaben wie ,02 

ct2eur :: Int -> String
ct2eur n 
   | n > 99999 = show (n `div` 100000) ++ "." ++ (take 3 (last5 (show n))) ++ "," ++ (take 2 (last2 (show n)) )
   | n > 99 = show (n `div` 100) ++ "," ++ (take 2 (last2 (show n)) )
   | n > 9 = "0," ++ show n
   | n >= 0 = "0,0" ++ show n
   | otherwise = error"ct2eur n nur n >= 0 definiert"


-- returns suffix of length 5 bzw. 2 (pre: Listenlaenge >= 5 bzw. 2) 

last5 :: [a] -> [a]
last5 l  
  | length l < 5 = error"Liste zu kurz"
  | otherwise = drop (length l - 5) l
 
last2 :: [a] -> [a]
last2 l  
  | length l < 2 = error"Liste zu kurz"
  | otherwise = drop (length l - 2) l


-- Summe zu einem Posten (in Ct) berechnen

sumP :: Posten -> Int
sumP (anz, ware, einzelPr) 
   | (anz < 0 || anz > 999) = error"Anzahl nicht zwischen 0 und 999! sP"
   | einzelPr < 0 = error"Einzelpreis negativ! (sumP)"
   | einzelPr > 99999 = error"Einzelpreis zu hoch! (sumP)"
   | anz*einzelPr > 9999999 = error"Anzahl * Einzelpreis zu gross! sP"
   | otherwise = anz*einzelPr


-- bringt eine Rechnung der Breite lg auf den Bildschirm

printRechnung :: Rechnung -> Int -> IO()
printRechnung rechng lg = putStr (makeRechnung rechng lg)


-- makeRechnung :: Rechnung -> Int -> String
-- Erzeugt aus einer Rechnung der Breite lg einen String;
-- wird von printRechnung benutzt.
-- Fuegt Kopfzeilen, die Rechnungsposten-Zeilen ("body"), 
-- Zeilen fuer die Netto-Summe, die MW-Steuer
-- und den zu zahlenden Betrag sowie Fusszeilen zusammen.
-- verschiedene Hilfsfunktionen, u.a. fst3/snd3/trd3 zu Tripeln
-- Mindestbreite e. Rechnung: 28 Zeichen (dann keine Warenbez. moegl.)
-- Rechnungen mit Posten, die eine zu lange Warenbezeichnung 
-- enthalten, fuehren ebenfalls zu einer Fehlermeldung.


makeRechnung :: Rechnung -> Int -> String
makeRechnung r lg  
   | (lg - 28 < 0) = error"Mindestbreite e. Rechnung ist 28!"
   | ([p | p <- r, length (snd3 p) > (lg - 28) ] /= []) = error"Es ex. Posten mit zu langem Warennamen!"
   | otherwise = kopf lg ++ "\n" ++ body r lg ++ zwStr lg ++ nettoSumL r lg ++ mwSt16L r lg ++ endStr lg ++ bruttoSumL r lg ++ "\n" ++ fuss



kopf :: Int -> String  -- Zeile der Laenge n mit zentrierter Ausgabe
kopf n = replicate ((n - 28)`div` 2) ' ' ++ "Haskell-Phantasie-Restaurant \n"  -- 28 Zeichen lang


fuss :: String
fuss = "Einen guten Heimweg und auf Wiedersehn! \n"  -- 39 Zeichen


body :: Rechnung -> Int -> String -- erzeugt den Postenteil der Rechnung
body r lg    
   | lg - 28 < 0 = error"Mindestbreite e. Rechnung ist 28! (body)"
   | ([p | p <- r, length (snd3 p) > lg - 28 ] /= []) = error"Es ex. Posten mit zu langem Warennamen! (body)"
   | otherwise = concat [(makeLine p lg) | p <- r ]



-- weitere Berechnungen 

nettoSum :: Rechnung -> Int   -- Rechnungssumme in Ct
nettoSum r = sum [fst3 p * trd3 p | p <- r]

fst3 :: (a,b,c) -> a
fst3 (x, y, z) = x

snd3 :: (a,b,c) -> b
snd3 (x, y, z) = y

trd3 :: (a,b,c) -> c
trd3 (x, y, z) = z


mwSt16 :: Rechnung -> Int   -- berechnet MW-St (16 %) in Ct
mwSt16 r = round ( (fromInt (nettoSum r)) / 25 * 4.0)


bruttoSum :: Rechnung -> Int
bruttoSum r = nettoSum r + mwSt16 r 


-- Formatierungen der Netto-, der mwSt- und der brutto-Zeilen
-- Texte rechtsbuendig (Ende in der Spalte 12 v. rechts)
-- Zahlen ab Spalte 11 v. re ebenfalls rechtsbuendig

nettoSumL :: Rechnung -> Int -> String -- Zeile mit RechnungsNetto
nettoSumL r lg 
   | bruttoSum r >= 99999999 = error"Rechnung nettoSum zu hoch!"  -- 999.999,99 EUR: 11 Char + (<= 17) <= 28 = Mindestbreite (netto)
   | lg - 28 < 0 = error"Mindestbreite e. Rechnung ist 28! (netto)"
   | ([p | p <- r, length (snd3 p) > lg - 28 ] /= []) = error"Es ex. Posten mit zu langem Warennamen! (netto)"
   | otherwise = replicate (lg - 14 - 11) ' ' ++ "Netto (EUR): " ++ replicate ( 11 - length (ct2eur (nettoSum r)) ) ' ' ++ ct2eur (nettoSum r) ++ "\n"


mwSt16L :: Rechnung -> Int -> String  -- Zeile mit MWSt
mwSt16L r lg 
   | bruttoSum r >= 99999999 = error"Rechnung MWST zu hoch!"  -- 999.999,99 EUR: 11 Char (mwSt)
   | lg - 28 < 0 = error"Mindestbreite e. Rechnung ist 28! (MWSt)"
   | ([p | p <- r, length (snd3 p) > lg - 28 ] /= []) = error"Es ex. Posten mit zu langem Warennamen! (MWSt)"
   | otherwise = replicate (lg - 18 - 11) ' ' ++ "MWSt (16%, EUR): " ++ replicate (11 - length (ct2eur (mwSt16 r))) ' ' ++ ct2eur (mwSt16 r) ++ "\n"


-- Zeile mit Rechnungsbrutto

bruttoSumL :: Rechnung -> Int -> String 
bruttoSumL r lg  
   | (bruttoSum r >= 99999999) = error"Rechnung bruttoSum zu hoch!"  -- 999.999,99 EUR: 11 Char (brutto)
   | lg - 28 < 0 = error"Mindestbreite e. Rechnung ist 28! (brutto)"
   | ([p | p <- r, length (snd3 p) > lg - 28 ] /= []) = error"Es ex. Posten mit zu langem Warennamen! (brutto)"
   | otherwise = replicate (lg - 18 - 11) ' ' ++ "zu zahlen (EUR): " ++ replicate (11-length (ct2eur (bruttoSum r)) ) ' ' ++ ct2eur (bruttoSum r) ++ "\n"

   
-- Zwischensummenlinie und Endsummenlinie (Zeilenlaenge lg)

zwStr :: Int -> String
zwStr lg 
   | (lg < 28 || lg > 70) = error"Falsche Laenge (zwStr)"
   | otherwise = replicate (lg - 11) ' ' ++ replicate 11 '-' ++ "\n"

   
endStr :: Int -> String
endStr lg
   | (lg < 28 || lg > 70) = error"Falsche Laenge (endStr)"
   | otherwise = replicate (lg - 11) ' ' ++ replicate 11 '=' ++ "\n"   


-- gibt myRechnung aus
-- diese Funktion bitte in jeder Lösung definieren!

dieRechnungBitte :: IO()
dieRechnungBitte = printRechnung myRechnung 70




{- ***************************
*** Testlauf ***

Main> dieRechnungBitte
                     Haskell-Phantasie-Restaurant

  5 Koepi zu je 2,20: ......................................    11,00
  1 Pommes zu je 1,80: .....................................     1,80
999 Superbilliges fast-Geschenk-Essen zu je 0,05: ..........    49,95
 77 Wahnsinnsteures Luxusessen zu je 999,99: ...............76.999,23
                                                           -----------
                                             Netto (EUR):   77.061,98
                                         MWSt (16%, EUR):   12.329,92
                                                           ===========
                                         zu zahlen (EUR):   89.391,90

Einen guten Heimweg und auf Wiedersehn!

Main>          

-}