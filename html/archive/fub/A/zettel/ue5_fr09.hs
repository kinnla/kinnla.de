{-=========================================================
                       5.Übung ALP 1            
      		 Sebastian Quick / Ewa Kampa
          		 Gruppe fr9                                    
=========================================================-}

----------------------- Aufgabe 3 -----------------------

-- Zum Testen bitte "dieRechnungBitte" aufrufen und einen der Beispiellisten verwenden (gast1..gast5) {wobei gast1 die Daten vom Zettel enthalten}

type Name    = String
type Preis    = Float
type Anzahl    = Int
type Rechnung   = [(Anzahl,Name,Preis)]

-- ========== Testdaten ==========

-- Rechnung vom Übungsbogen
gast1 :: Rechnung
gast1 = [(5,"Königs Pilsener",2.6),(1,"Pommes mit Mayo",2.1)]

-- Rechnbung von Gast2 (Einzelgast)
gast2 :: Rechnung
gast2 =  [(2,"Bier",4),(1,"Schnitzel",6.5),(1,"Salat",3.25),(1,"Tiramisu",4)]

-- Rechnung von Gast3 (Familie mit 2 Kindern)
gast3 :: Rechnung
gast3 = [(2,"Cola",3.5),(1,"Bier",2),(1,"Mineralwasser",1.5),(2,"Kinderpizza",7),(1,"Pizza Salami",4.5),(1,"Pizaa Tonno",5),(2,"Eis",5),(2,"Cappucino",4)]

-- Rechnung von Gast4 (Betriebsfeier)
gast4 :: Rechnung
gast4 = [(200,"Bier",2.6),(200,"Wein",3),(150,"sonstige Getränke",2),(1,"Buffet",750)]

-- Rechnung von Gast5 (Testrechnung um verschiedene Werte zu sehen)
gast5 :: Rechnung
gast5 = [(10,"Test1",10.23),(5,"Test2",0.5),(100,"Test3",3.09),(23,"Test4",100)]

-- ===============================

-- Funktion formatiert den freien Platz für den Preis
formatPreis:: Preis -> [Int]
formatPreis(p)
 | p <  10 = [0,1,2]
 | p < 100 = [0,1]
 | p < 1000 = [0]
 | p < 10000 = [] 
 | otherwise = error"So teuer kann das doch gar nicht sein"

-- Funktion formatiert den freien Platz für die "Anzahl"
formatAnzahl:: Anzahl -> [Int]
formatAnzahl(a)
 | a <  10 = [0,1]
 | a < 100 = [0]
 | a < 1000 = []
 | otherwise = error"So viele Dinge kann es nicht geben"

-- Funktion hilft bei der Formatierung der Preise 
formatPreish :: Float->String
formatPreish p = if (mod(round(p*100)) 100) < 10 then  show(div(round(p*100))100)++".0"++ show(mod (round((p*100)))100)
      else  show(div (round(p*100))100)++"."++show(mod (round(p*100))100)


-- Funktion formatiert eine Zeile der Rechnung
formatLine:: (Anzahl,Name,Preis) -> String
formatLine (a,n,p)=(replicate (length(formatAnzahl(a))) ' '++show(a)++" x "++n++replicate (20-(length(n))) '.'++replicate (length(formatPreis(p))) ' ' ++(formatPreish(p))++" €\n" )

-- Funktion erstellt den gesamten oberen Teil der Rechnung        
formatLines :: [(Anzahl,Name,Preis)] -> String
formatLines[]=[]
formatLines(x:xs)=formatLine(x)++formatLines(xs)

-- Funktion rechnet den Gesamtpreis der Rechnung aus
makeTotal::Rechnung->Float
makeTotal s = sum[ps(p)|p<-s]
         where ps :: (Anzahl,Name,Preis) -> Float
               ps (a, _, p) = (fromInt(a) * p)

-- Funktion gibt den Preis mit MwSt aus
mehrWert ::Rechnung -> String
mehrWert gast = show(div (round(((makeTotal(gast)*16)/100+(makeTotal(gast)))*100)) 100)++"."++(mehrWerth(gast))

-- Funktion hilft bei der Formatierung der Mehrwertsteuer
mehrWerth :: Rechnung->String
mehrWerth gast = if (mod(round(((makeTotal(gast)*16)/100+(makeTotal(gast)))*100)) 100) < 10 then  "0"++show(mod(round(((makeTotal(gast)*16)/100+(makeTotal(gast)))*100)) 100)
      else show(mod(round(((makeTotal(gast)*16)/100+(makeTotal(gast)))*100)) 100)

-- Funktion gibt den Rechnungsbetrag ohne MwSt und mit MwSt aus 
formatTotal :: Rechnung->String
formatTotal gast = 
 replicate 35 '-'++"\n"++"Total (o.MwSt)"++replicate 12 '.'++replicate (length(formatPreis(makeTotal(gast)))) ' '++formatPreish(makeTotal(gast))++" €\n"
  ++"Total (m.MwSt)"++replicate 12 '.'++replicate (length(formatPreis(makeTotal(gast)))) ' '++mehrWert(gast)++" €\n"

-- Funktion fügt die beiden Teilstrings der Rechnung zusammen
zusammen:: Rechnung->String
zusammen gast = "\n"++"\t"++"Die Rechnung\n"++"\n"++formatLines(gast)++formatTotal(gast)++"\n"

-- Funktion gibt die formatierte Rechnung aus
dieRechnungBitte :: Rechnung -> IO () 
dieRechnungBitte gast = putStr(zusammen(gast))

{-
Die verschiedenen Funktion rufen sich unter einander gegenseitig auf um die Eingegebenen Daten in Strings zu verwandeln und entsprechend 
zu formatieren.
Als Formatierung treten auf:
Float: ein Float der Form: 2.1 wird nach der Formatierung als 2.10 ausgegeben
       Bei der Mehrwertsteuer kommt es durch die 16% zu Zahlen, die mehr als 2 Nachkommastellen
       haben. Hier wird gerundet und der Gesamtpreis mit Mehrwertsteuer ausgegeben.
Eine weitere Formatierung geschieht über Platzhalter:
Die "Anzahl" kann in unserem Fall maximal 999 betragen.
Der "Preis" kann maximal 9999 € betragen
Der "Gesamtpreis" kann ebenfalls maximal 9999 € betragen
Dies kann vergrössert werden durch das Ändern der Funktionen formatPreis und formatAnzahl
Zur Formatierung der Ausgabe wurden replicate, ++, \n und \t verwendet
-}


{-
---------- Testläufe ---------

Main>                       
Main> dieRechnungBitte gast3

        Die Rechnung

  2 x Cola................   3.50 €
  1 x Bier................   2.00 €
  1 x Mineralwasser.......   1.50 €
  2 x Kinderpizza.........   7.00 €
  1 x Pizza Salami........   4.50 €
  1 x Pizaa Tonno.........   5.00 €
  2 x Eis.................   5.00 €
  2 x Cappucino...........   4.00 €
-----------------------------------
Total (o.MwSt)............  52.00 €
Total (m.MwSt)............  60.32 €


Main>                                       

-}
           