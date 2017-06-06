{-

Michaela Schulz
Stefan Lenz


|
|
|                  ______________________
|                 |          b2          |
|                 |                      |
|                 |                      |
|                 |      _________       |
|                 |     |    b1   |      |
|                 |h2   |h1       |      |
|                 |     |         |      |
|                 |   y1|_________|      |
|                 |     x1               |
|                 |                      |
|               y2|______________________|
|                 x2
|
|
|
|_________________________________________________


Wie man der Skizze entnehmen kann ist R1=(x1,y1,h1,b1) ganz in 
R2=(x2,y2,h2,b2) enthalten, wenn der linke untere Punkt (x1,y1) und 
der rechte obere Punkt (x1+b1,y1+h1) von R1 in R2 enthalten ist.
Dies ist der Fall, wenn alle folgende Ungleichungen gelten :
x1 > x2
y1 > y2
x1+b1 < x2+b2
y1+h1 < y2+b2

Zunächst wird noch geprüft, ob die Höhen und Breiten auch wie gefordert grösser null sind.
-}

enth ::((Int,Int,Int,Int),(Int,Int,Int,Int)) -> Bool
enth((x1,y1,h1,b1),(x2,y2,h2,b2)) 
  |(h1<0)||(b1<0)||(h2<0)||(b2<0) = error "Falsche Eingabe"
  |otherwise = (x1>x2) && (y1>y2) && (x1+b1<x2+b2) && (y1+h1<y2+h2)

{- Testläufe :

Main> enth ((5,5,3,3),(1,1,10,10))
True
Main> enth ((5,5,10,3),(1,1,10,10))
False      
Main> enth ((5,5,10,3),(1,1,10,-10))

Program error: Falsche Eingabe   

-}



{- 
Laut Definition ist ein Jahr ein Schaltjahr, wenn es durch 4 aber nicht durch 100
teilbar ist, oder wenn es durch 400 teilbar ist.

-}

schalt :: Int -> Bool
schalt (jahr) = (mod jahr 4 == 0) && (mod jahr 100 >0) || (mod jahr 400 == 0)

{- Testläufe : 

Main> schalt (2002)
False
Main> schalt (2000)
True
Main> schalt (1900)
False
Main> schalt (1904)
True                                   

-}


{- 
Die Nummer eines Tages erhält man, indem man die Tage die bis zum Beginn des
Monats vergangen sind zum Tag hinzuaddiert.

Die Eingabe eines ungültigen Datums provoziert eine Fehlermeldung
-}

tagesNr :: (Int,Int) -> Int
tagesNr (tag,monat)
  | (monat==1)  && (tag <=31) = tag
  | (monat==2)  && (tag <=28) = 31+tag
  | (monat==3)  && (tag <=31) = 59+tag
  | (monat==4)  && (tag <=30) = 90+tag
  | (monat==5)  && (tag <=31) = 120+tag
  | (monat==6)  && (tag <=30) = 151+tag
  | (monat==7)  && (tag <=31) = 181+tag
  | (monat==8)  && (tag <=31) = 212+tag
  | (monat==9)  && (tag <=30) = 243+tag
  | (monat==10) && (tag <=31) = 273+tag
  | (monat==11) && (tag <=30) = 304+tag
  | (monat==12) && (tag <=31) = 334+tag
  | otherwise = error "Ungültiges Datum"
  
{- Testläufe :
  
Main> tagesNr(1,1)
1
Main> tagesNr(1,2)
32
Main> tagesNr(31,12)
365
Main> tagesNr(29,2)

Program error: Ungültiges Datum        
-}

{-
Um den Wochentag eines Datums zu erhalten, muss man zunächst die Anzahl der Tage
die seit Montag dem 01.01.01 vergangen sind berechnen :

#Tage_seit_01_01_01 = #Vergangene_Jahre*365 + #Vergangene_Schaltjahre + #Tage_im_lfd_Jahr-1

#Vergangene_Jahre = jahr-1
#Vergangene_Schaltjahre = #Vergangene_Jahre_die_durch_4_teilbar_sind 
              -#Vergangene_Jahre_die_durch_100_teilbar_sind
              +#Vergangene_Jahre_die_durch_400_teilbar_sind
              
#Tage_im_lfd_Jahr = tagesNr(tag,monat) + 1, falls laufendes Jahr ein Schaltjahr ist
                                            und Datum im März oder später liegt
                                            bzw =60 falls es sich um den 29.2. handelt, da tagesNr() 
                                            nur für Schaltjahre definiert ist.

Der Rest der Division der erhaltenen Tage-Anzahl durch 7 beschreibt den Wochentag.
Addiert man noch flugs eins hinzu, so steht die 1 für Montag und die 7 für Sonntag


Falsche Daten erzeugen eine entsprechende Fehlermeldung in tagesNr()
-}

anzahl_Vergangene_Schaltjahre :: Int -> Int
anzahl_Vergangene_Schaltjahre(jahr) = (div jahr 4) - (div jahr 100) + (div jahr 400)


anzahl_Tage_seit_01_01_01 :: (Int,Int,Int) -> Int
anzahl_Tage_seit_01_01_01 (tag,monat,jahr)
   | (monat > 2 ) && schalt(jahr) = 1+ (jahr-1)*365 + anzahl_Vergangene_Schaltjahre(jahr-1) + tagesNr(tag,monat)-1    
   | (monat == 2 ) && (tag == 29) && schalt(jahr) = 60+ (jahr-1)*365 + anzahl_Vergangene_Schaltjahre(jahr-1) -1    
   | otherwise = (jahr-1)*365 + anzahl_Vergangene_Schaltjahre(jahr-1) + tagesNr(tag,monat)-1
    
kalender :: (Int,Int,Int) -> Int
kalender (tag,monat,jahr) = (mod (anzahl_Tage_seit_01_01_01(tag,monat,jahr)) 7 ) +1


{- Testläufe :

Main> kalender(3,11,2002)
7
Main> kalender(4,11,2002)
1
Main> kalender(11,9,2001)
2
Main> kalender(13,2,1948)
5                              
Main>  kalender(29,2,2000)
2
Main>  kalender(29,2,2002)

Program error: Ungültiges Datum    
-}