{-

Stephanie Wilke / Bodo Eichstädt (Gruppe 1)

Alp1 - Übungsblatt 2 

07.11.2002
-}

---------------------------------------------------------------------------
------------------------- Aufgabe 1 ---------------------------------------
---------------------------------------------------------------------------


module Alp1u2g1 where

{----------------------------------------------------------------------------------------------
Diese Funktion prüft, ob bei zwei gegebenen Rechtecken das eine in dem anderen enthalten ist.

Eingabe:  Rechteck 1: x1,y1 = koordinate der linke unteren Ecke
                      b1,h1 = Breite und Höhe des Rechteckes

          Rechteck 2: x2,y2 = Koordinate der linke unteren Ecke
                      b2,h2 = Breite und Höhe des Rechteckes

Rückgabe: R1 ganz in R2 enthalten => True 
          sonst                   => False
----------------------------------------------------------------------------------------------}
enth :: ((Int,Int,Int,Int),(Int,Int,Int,Int)) -> Bool
enth ((x1,y1,b1,h1), (x2,y2,b2,h2)) =                  --

                                                -- wir betrachten die Koordinaten erst der linken unteren Ecken...
  x1 >= x2  &&  y1 >= y2                        --   "Sind die Koordinaten x und y von R1 jeweils grösser als die von R2 ?"
  &&                                            -- ... und dann der rechten oberen Ecke
  (x1+b1) <= (x2+b2)  &&  (y1+h1) <= (y2+h2)    --   "Um die Koordinaten der rechten oberen Ecke beider Rechtecke jeweils zu bekommmen, addiert man jeweils
                                                --    x-Koord. und Breite und y-Koord. und Höhe und vergleicht dann diese Punkte ähnlich wie die linke 
                                                --    untere Ecke."






---------------------------------------------------------------------------
------------------------- Aufgabe 2 ---------------------------------------
---------------------------------------------------------------------------




-- a) --


{-------------------------------------------------------

Diese Funktion prüft, ob ein Jahr j ein Schaltjahr ist.

Eingabe:    j = ganzzahlige Jahreszahl

Ausgabe     j ist  ein Schaltjahr => True
            j ist kein Schaltjahr => False

-------------------------------------------------------}
schalt :: Int -> Bool
schalt j = 
         (       (j `mod` 4   == 0)                               -- Alle 4 Jahre ein Schaltjahr...
           && not(j `mod` 100 == 0)                               -- ... ausser zur Jahrhundertwende.
         ) ||    (j `mod` 400 == 0)                               -- Aber alle 400 Jahre ist doch wieder ein Schaltjahr.

-- b) --

{--------------------------------------------------------------------------------------------------------------
Für einen gegebenen Tag und Monat eines (Nicht-Schalt)Jahres ermitteln, um den wievielten Tag es sich handelt.

Eingabe:    t = Tag im Monat   (1..31)
            m = Monat im Jahr  (1..12)

Rückgabe:   Anzahl der Tage im Jahr bis zum gegebenen Tag & Monat
---------------------------------------------------------------------------------------------------------------}
tagesNr :: (Int,Int) -> Int
tagesNr (t,m) 
                      | t>tageImMonat1(m)    = error ("tagesNr: Es gibt das Datum "                -- Gibt es im Monat m überhaupt t Tage ?
                                                      ++show(t) ++ "." ++ show(m) ++ ". nicht!")
                      | m==1                 = t                                                   -- Januar hat keinen Vorgänger
                      | otherwise            = t + tagesAnzahlMonate1(m-1)                         -- Summe der Tage alle vorhergenden Monate + Anzahl
                                                                                                   --  der Tage t im Monat m




-- Hilfsfunktionen --


{-------------------------------------------------------------------------------------------
Anzahl der Tage nach der ersten m Monate eines (nicht-Schalt)jahres

Eingabe:     m = Nummer eines Monats
Rückgabe:    Summe der Tage aller Monate vom 1. bis m-ten Monat des Jahres (einschließlich)

--------------------------------------------------------------------------------------------}
tagesAnzahlMonate1 :: Int -> Int
tagesAnzahlMonate1 m
                      | m==0                =  0
                      | m==1                =  tageImMonat1(1)                                 -- Januar hat keine Vorgänger (Verankerung der Rekursion)
                      | m>1 && m<=12        =  tagesAnzahlMonate1(m-1) + tageImMonat1(m)       -- Alle anderen Monate: Aufsummieren der Tage/Monat per Rekursion
                      | otherwise           =  error("tagesAnzahlMonate1: " ++ show(m)         -- ungültige Monate
                                                     ++ " ist kein gültiger Monat !!") 




{-------------------------------------------------------------------------------------------
Anzahl der Tage in einem angegebenen Monat bestimmen.

Eingabe:    m = Nummer des Monats, für den die Anzahl der Tage bestimmt werden soll
Rückgabe:   
-------------------------------------------------------------------------------------------}
tageImMonat1 :: Int -> Int
tageImMonat1 m  
                      | (m<1 || m>12)                   = error("tageImMonat1: " ++ show(m) ++ "ist kein gültiger Monat!") 
                      | m==2                            = 28              -- Februar              hat   28 Tage
                      | m==4 || m==6 || m==9 || m==11   = 30              -- Aril, Juni, Sep, Nov haben 30  "
                      | otherwise                       = 31              -- alle anderen Monate   "    31  "







---------------------------------------------------------------------------
------------------------- Aufgabe 3 ---------------------------------------
---------------------------------------------------------------------------




{--------------------------------------------------------------------------------------------
Berechnet zu einem angegebenen Datum (Tag, Monat, Jahr) um welchen Wochentag es sich handelt.

Eingabe:  t = Tag im Monat   [1..31]
          m = Monat im Jahr  [1..12]
          j = Jahr

Rückgabe: Wochentag-Nummer (1=Montag, 2=Dienstag, ... , 7=Sonntag)

---------------------------------------------------------------------------------------------}
kalender :: (Int, Int, Int) -> Int
kalender (t,m,j) = ((tage(t,m,j) - 1) `mod` 7) + 1    -- Anzahl der Tage seit Zeitrechnung Modulo 7 (die Woche hat 7 Tage)
                                                      -- Die Verschiebung -1 und +1 Transformiert die Skaala von
                                                      -- 0=Sonntag, 1=Montag, ... , 6=Samstag
                                                      -- nach
                                                      -- 1=Montag, 2=Dienstag, ... , 7=Sonntag





{-----------------------------------------------------------------------
Anzahl der Tage zu einem Datum (Tag, Monat, Jahr) seit der Zeitrechnung.

Eingabe:  t = Tag im Monat   [1..31]
          m = Monat im Jahr  [1..12]
          j = Jahr

Rückgabe: Anzahl der Tage (inkl. Schaltjahre)

------------------------------------------------------------------------}
tage :: (Int, Int, Int) -> Int
tage (t,m,j) = 
                 ((j-1) * 365)                 -- Wieviele Tage haben alle vorhergenden Jahre zusammen (ohne Schalttage) ?
               + anzahlSchaltjahre(j-1)        -- Wieviele Tage kommen durch Schaltjahre (in den vorhergegangenen Jahren) dazu ?
               + tagesNr2(t,m,j)               -- Wieviele Tage kommen im aktuellen Jahr in den vorhergendenen Monaten & Tagen hinzu (inkl. ggf. Schalttag) ?





{---------------------------------------------------------------------
Berechnet die Gesamtzahl der Schaltjahre bis zu einem angegebenen Jahr

Eingabe:   j = Jahreszahl

Rückgabe:  Anzahl der Schaltjahre

----------------------------------------------------------------------}
anzahlSchaltjahre :: Int -> Int
anzahlSchaltjahre j = 
            j `div` 4             -- Alle 4   Jahre ist ein  Schaltjahr
          - j `div` 100           -- alle 100 Jahre ist kein Schaltjahr
          + j `div` 400           -- Alle 400 Jahre ist ein  Schaltjahr 






-- folgende Hilfsfunktionen ersetzen sinngemäss jene aus Aufgabe 2 weil sie Schaltjahre berücksichtigen --




{-------------------------------------------------------------------------
Anzahl der Tage nach der ersten m Monate eines Jahres (inkl. Schaltjahre)

Eingabe:     m = Nummer eines Monats [1..12]
             j = Jahreszahl

Rückgabe:    Summe der Tage aller Monate von 1...m

--------------------------------------------------------------------------}
tagesAnzahlMonate2 :: (Int,Int)->Int
tagesAnzahlMonate2 (m,j)
                      | m==0                =  0
                      | m==1                =  tageImMonat2(1, j)                              -- Januar hat keine Vorgänger (Verankerung der Rekursion)
                      | m>1 && m<=12        =  tagesAnzahlMonate2(m-1, j) + tageImMonat2(m,j)  -- Alle anderen Monate: Aufsummieren der Tage/Monat per Rekursion
                      | otherwise           =  error("tagesAnzahlMonate2: " ++ show(m)         -- ungültige Monate
                                                     ++ "ist kein gültiger Monat !!")




{---------------------------------------------------------------------------------------------
Anzahl der Tage in einem angegebenen Monat bestimmen. (Schaltjahre _werden_ berücksichtigt!!!)

Eingabe:    m = Monats, für den die Anzahl der Tage bestimmt werden soll [1..12]
            j = Jahreszahl

Rückgabe:   Anzahl der Tage für den Monat
----------------------------------------------------------------------------------------------}
tageImMonat2 :: (Int,Int) -> Int
tageImMonat2 (m,j)
                      | (m<1 || m>12)                   = error("tageImMonat2: " ++ show(m) ++ "ist kein gültiger Monat!") 
                      | m==2 && schalt(j)               = 29              -- Februar im Schaltjahr        hat      29 Tage
                      | m==2 && not(schalt(j))          = 28              -- Februar im nicht-Schaltjahr  hat      28 Tage
                      | m==4 || m==6 || m==9 || m==11   = 30              -- Aril, Juni, Sep, Nov         haben je 30  "
                      | otherwise                       = 31              -- alle anderen Monate           "     " 31  "





{-------------------------------------------------------------------------------------------------
Für einen gegebenen Tag und Monat eines Jahres ermitteln, um den wievielten Tag es sich handelt.
Diese modifizierte Version jener aus Ausgabe 2, die auch Schaltjahre berücksichtigt!!!


Eingabe:    t = Tag im Monat   (1..31)
            m = Monat im Jahr  (1..12)
            j = Jahreszahl

Rückgabe:   Anzahl der Tage im Jahr bis zum gegebenen Tag & Monat
-------------------------------------------------------------------------------------------------}
tagesNr2 :: (Int,Int,Int) -> Int
tagesNr2 (t,m,j) 
                      | t>tageImMonat2(m,j)  = error ("tagesNr2: Es gibt keinen " ++ show(t)    -- Gibt es im Monat m überhaupt t Tage ? => Fehler
                                                      ++ "." ++ show(m) ++ "." ++ show(j)) 
                      | m==1                 = t                                                -- Januar hat keinen Vorgänger
                      | otherwise            = t + (if m>1 then tagesAnzahlMonate2(m-1,j) else 0)        -- Summe der Tage alle vorhergenden Monate + Anzahl
                                                                                                --  der Tage t im Monat m






-- optionale Funktionen für eine dt. Klartextdarstellung --



{--------------------------------------------------------
Einen gegebene nummerischen Monat als dt. Text liefern.

Eingabe:   m = Monat [1..12]

Rückgabe:  Monatsname
----------------------------------------------------------}
monatsname :: Int -> [Char]
monatsname m
             | m==1      = "Januar"
             | m==2      = "Februar"
             | m==3      = "März"
             | m==4      = "April"
             | m==5      = "Mai"
             | m==6      = "Juni"
             | m==7      = "Juli"
             | m==8      = "August"
             | m==9      = "September"
             | m==10     = "Oktober"
             | m==11     = "November"
             | m==12     = "Dezember"
             | otherwise = error("monatsname: Es gibt nur die Monate 1-12!")




{---------------------------------------------------------------------------
Gibt zu einer nummerischen Wochentagnummer, den Wochentag als dt. Text.

Eingabe:  w = Wochentagnummer [1..7] (1=Montag, 2=Dienstag, ..., 7=Sonntag)

Rückgabe: Wochentag als Text

----------------------------------------------------------------------------}
wochentag :: Int -> [Char]
wochentag w
        | w==1   = "Montag"
        | w==2   = "Dienstag"
        | w==3   = "Mittwoch"
        | w==4   = "Donnerstag"
        | w==5   = "Freitag"
        | w==6   = "Samstag"
        | w==7   = "Sonntag"




----------------------------------------------------------------------------
------------------------- Testläufe ----------------------------------------
----------------------------------------------------------------------------

{-


zu Aufgabe 1:

Fall 1: R1,R2 mit R1 vollständig in R2

Alp1u2> enth((5,5,5,5),(1,1,10,10))
True

Alp1u2> enth((5,5,5,5),(3,3,10,10))
True

Alp1u2> enth((5,5,5,5),(5,5,5,5))
True


Fall 2: R1,R2 mit R1 und R2 überlappen sich

Alp1u2> enth((5,5,5,5),(7,7,10,10))
False


Fall 3: R1,R2 mit R1 enthält/überlappt/berührt R2 nicht

Alp1u2> enth((5,5,5,5),(15,15,10,10))
False


Fall 4: R1,R2 mit R2 enthält zwar R1, aber nicht umgekehrt

Alp1u2> enth((5,5,5,5),(6,6,1,1))
False



zu Aufgabe 2:

a)

Alp1u2> schalt 1999
False
Alp1u2> schalt 2000
True
Alp1u2> schalt 2001
False
Alp1u2> schalt 2002
False
Alp1u2> schalt 2003
False
Alp1u2> schalt 2004
True



Zeige alle Schaltjahr zwischen 1500 und 2002, d.h. alle Aufrufe von "schalt" die True liefern

Alp1u2> filter schalt [1500..1600]
[1504,1508,1512,1516,1520,1524,1528,1532,1536,1540,1544,1548,1552,1556,1560,1564,1568,1572,1576,1580,1584,1588,1592,1596,1600]

Alp1u2> filter schalt [1600..1700]
[1600,1604,1608,1612,1616,1620,1624,1628,1632,1636,1640,1644,1648,1652,1656,1660,1664,1668,1672,1676,1680,1684,1688,1692,1696]

Alp1u2> filter schalt [1700..1800]
[1704,1708,1712,1716,1720,1724,1728,1732,1736,1740,1744,1748,1752,1756,1760,1764,1768,1772,1776,1780,1784,1788,1792,1796]

Alp1u2> filter schalt [1800..1900]
[1804,1808,1812,1816,1820,1824,1828,1832,1836,1840,1844,1848,1852,1856,1860,1864,1868,1872,1876,1880,1884,1888,1892,1896]

Alp1u2> filter schalt [1900..2002]
[1904,1908,1912,1916,1920,1924,1928,1932,1936,1940,1944,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000]


b)
Alp1u2> tagesNr(1,1)
1
Alp1u2> tagesNr(1,2)
32
Alp1u2> tagesNr(1,3)
60
Alp1u2> tagesNr(1,4)
91
Alp1u2> tagesNr(1,5)
121
Alp1u2> tagesNr(1,6)
152
Alp1u2> tagesNr(1,7)
182
Alp1u2> tagesNr(24,12)
358


Errors:
-- 32. Januar
Alp1u2> tagesNr(32,1)

Program error: tagesNr: Es gibt das Datum 32.1. nicht!

Alp1u2> tagesNr(29,2)

Program error: tagesNr: Es gibt das Datum 29.2. nicht!

Alp1u2> tagesNr(30,2)

Program error: tagesNr: Es gibt das Datum 30.2. nicht!

Alp1u2> tagesNr(31,2)

Program error: tagesNr: Es gibt das Datum 31.2. nicht!

Alp1u2> tagesNr(31,4)

Program error: tagesNr: Es gibt das Datum 31.4. nicht!


zu Aufgabe 3:

Alp1u2> kalender(7,11,2002)
4
Alp1u2> kalender(8,11,2002)
5
Alp1u2> kalender(9,11,2002)
6
Alp1u2> kalender(10,11,2002)
7
Alp1u2> kalender(11,11,2002)
1
Alp1u2> kalender(12,11,2002)
2
Alp1u2> kalender(13,11,2002)
3
Alp1u2> kalender(14,11,2002)
4


Alp1u2> kalender(1,1,1)
1
Alp1u2> kalender(6,12,2002)
5
Alp1u2> kalender(24,12,2002)
2
Alp1u2> kalender(31,12,2002)
2
Alp1u2> wochentag(kalender(6,12,2002))
"Freitag"
Alp1u2> wochentag(kalender(24,12,2002))
"Dienstag"
Alp1u2> wochentag(kalender(31,12,2002))
"Dienstag"


Alp1u2> wochentag(kalender(7,11,2002))
"Donnerstag"
Alp1u2> wochentag(kalender(9,11,2002))
"Samstag"
Alp1u2> wochentag(kalender(7,11,2002))
"Donnerstag"
Alp1u2> wochentag(kalender(8,11,2002))
"Freitag"
Alp1u2> wochentag(kalender(9,11,2002))
"Samstag"
Alp1u2> wochentag(kalender(10,11,2002))
"Sonntag"
Alp1u2> wochentag(kalender(11,11,2002))
"Montag"
Alp1u2> wochentag(kalender(12,11,2002))
"Dienstag"
Alp1u2> wochentag(kalender(13,11,2002))
"Mittwoch"
Alp1u2> wochentag(kalender(14,11,2002))
"Donnerstag"


Alp1u2g1> wochentag(kalender(29,2,2000))
"Dienstag"
Alp1u2g1> wochentag(kalender(29,2,2001))
"
Program error: tagesNr2: Es gibt keinen 29.2.2001

Alp1u2g1> wochentag(kalender(29,2,2002))
"
Program error: tagesNr2: Es gibt keinen 29.2.2002

Alp1u2g1> wochentag(kalender(29,2,2003))
"
Program error: tagesNr2: Es gibt keinen 29.2.2003

Alp1u2g1> wochentag(kalender(29,2,2004))
"Sonntag"


-}
