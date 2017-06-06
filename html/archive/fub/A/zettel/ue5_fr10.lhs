========================================================================

Algorithmen und Programmieren I

Uebungszettel 5
________________________________________________________________________
Benjamin Jankovic, Christian Mueller
Tutor: Till Zoppke, Fr. 8 - 10
========================================================================

Aufgabe 3:
________________________________________________________________________

Formatierung der Anzahl:
Je nach Anzahl der Dezimalstellen werden ein oder mehrere Leerzeichen 
vor die Zahl gesetzt.

>anzForm::Int->String
>anzForm a
>	| a<10	= "   "++show(a)
>	| a<100 = "  "++show(a)
>	| otherwise = " "++show(a)

Formatierung der Speisen:
Je nach Laenge des Strings muessen unterschiedlich viele Tabulatoren 
verwendet werden und Zeilenumbrueche eingefuegt werden. Bei mehr als 
vier Zeilen (mehr als 60 Zeichen) faellt der Rest des Strings weg.
Bei den Zeilenumbruechen muessen zusaetzlich noch Formatierungselemente 
eingefuegt werden, damit am Ende die Gesamtrechnung das richtige 
Erscheinungsbild hat.

>speiseForm::String->String
>speiseForm s
>	| length(s)<8	= s++"\t\t"
>	| length(s)<16  = s++"\t"
>	| length(s)<23	= (take 15 s)++"\t\t\t  *\n*\t"
>			++(take 15(drop 15 s))++"\t\t"
>	| length(s)<31	= (take 15 s)++"\t\t\t  *\n*\t"
>			++(take 15(drop 15 s))++"\t"
>	| length(s)<38 	= (take 15 s)++"\t\t\t  *\n*\t"
>			++(take 15(drop 15 s))++"\t\t\t  *\n*\t"
>			++(take 15(drop 30 s))++"\t\t"
>	| length(s)<46	= (take 15 s)++"\t\t\t  *\n*\t"
>			++(take 15(drop 15 s))++"\t\t\t  *\n*\t"
>			++(take 15(drop 30 s))++"\t"
>	| length(s)<53 	= (take 15 s)++"\t\t\t  *\n*\t"
>			++(take 15(drop 15 s))++"\t\t\t  *\n*\t"
>			++(take 15(drop 30 s))++"\t\t\t  *\n*\t"
>			++(take 15(drop 45 s))++"\t\t"
>	| otherwise	= (take 15 s)++"\t\t\t  *\n*\t"
>			++(take 15(drop 15 s))++"\t\t\t  *\n*\t"
>			++(take 15(drop 30 s))++"\t\t\t  *\n*\t"
>			++(take 15(drop 45 s))++"\t"

Formatierung des Preises (1):
Da die show-Funktion nur eine Nachkommastelle angibt, wenn die 2. oder 
folgende gleich 0 sind, wird geprueft, ob das vorletzte Zeichen von 
(show p) ein Punkt ist. In diesem Fall wird noch eine 0 dem String 
hinzugefuegt. Andernfalls aendert sich nichts.

>preisForm1 p
>	| head(tail(reverse(show(p))))=='.'	= show(p)++"0"
>	| otherwise				= show(p)

Formatierung des Preises (2):
Damit die 2 Nachkommastellen immer uebereinanderliegen, muessen alle 
Preise die selbe Laenge haben. Je nach Anzahl der Stellen im Preis 
werden also Leerzeichen vor dem String eingefuegt.
(Kleinste Laenge ist 4: 1 Vorkommastelle, der Punkt, 2 Nachkommastellen)
(Betraege groesser gleich 1000 werden nicht mehr richtig formatiert)

>preisForm2::String->String
>preisForm2 s
>	| length(s)==4	= "   "++s
>	| length(s)==5	= "  "++s
>	| otherwise		= " "++s

Zusammenfassung der beiden Formatierungen zu einer:

>showPreis::Float->String
>showPreis s = preisForm2(preisForm1 s)

Formatierung einer Zeile:
In einer Zeile stehen die Anzahl, Speise, Einzelpreis und
Summe = Anzahl * Einzelpreis durch Tabulatoren getrennt. Hierbei werden 
die jeweiligen bereits formatierten Strings eingesetzt.
Auch hier gehen noch ein paar Formatierungselemente in die Zeile ein.

>zeile::(Int,String,Float)->String
>zeile (a,s,p) = "* "++(anzForm a)++"\t"
>		++(speiseForm s)
>		++(showPreis p)++"\t"
>		++(showPreis(fromInt(a)*p))++"   *"

Formatierung des gesamten Rechnungsblocks:
Nun wird die Eingabeliste rekursiv abgearbeitet. Jedes Element wird als 
Zeile dargestellt und daran wird nach einem Zeilenumbruch der Rest der 
Liste, also die folgenden Zeilen, gehaengt.

>block::[(Int,String,Float)]->String
>block [] = []
>block ((a,s,p):xs) = zeile(a,s,p) ++ "\n" ++ block xs

Berechnung der Gesamtsumme:
Hierzu wird auch rekursiv vorgegangen. Der Preis fuer eine Speise mal 
der Anzahl wird zum Rest addiert.

>summe::[(Int,String,Float)]->Float
>summe [] = 0
>summe ((a,s,p):xs) = fromInt(a)*p + (summe xs)

Berechnung des Bruttopreises:
Der Bruttopreis wird berechnet, indem man 16% des Nettopreises zum 
Nettopreis selbst addiert. Damit das Ergebnis nur 2 Nachkommastellen 
hat, wird der Betrag mit 100 multipliziert, nach oben gerundet (nehm 
ich mal an) und wieder durch 100 geteilt.

>mwst::Float->Float
>mwst s = fromInt(ceiling(100*(s + s*0.16)))/100

Die Rechnung:
Nun werden die Einzelbausteine, die wir haben, zusammengesetzt und in 
einen Rahmen gebracht.

>rechnung::[(Int,String,Float)]->IO()
>rechnung x = putStr ("\n\n\n"
>		++"* * * * * * * * * * * * * * * * * * * * * *\n"
>		++"*                                         *\n"
>		++"*          Gasthof zum ALPtraum           *\n"
>		++"*         Inhaber:  B.J. und C.M.         *\n"
>		++"*                                         *\n"
>		++"* * * * * * * * * * * * * * * * * * * * * *\n"
>		++"* Rechnung:                      28.11.02 *\n"
>		++"*                                         *\n"
>		++"* Anz.  Speise           Einzel   Summe   *\n"
>		++"* --------------------------------------- *\n"
>		++(block x)
>		++"* --------------------------------------- *\n"
>		++"* Gesamtsumme:\t\t\t"
>		++(showPreis(summe x))++"   *\n"
>		++"* zzgl. 16% Mwst.\t\t"
>		++(showPreis(mwst(summe x)))++"   *\n"
>		++"*                                ======   *\n"
>		++"* (alle Preise in Euro)                   *\n"
>		++"* * * * * * * * * * * * * * * * * * * * * *\n"
>		++"* Vielen Dank fuer Ihren Besuch!          *\n"
>		++"* * * * * * * * * * * * * * * * * * * * * *\n")

Um nicht jedesmal beim Testen alle Eingaben machen zu muessen, kann man 
noch eine Konstante definieren. (myrechnung)
dieRechnungBitte setzt diese Konstante in die Funktion 'rechnung' ein 
und gibt das Ergebnis aus.

>type Posten = (Int,String,Float)
>type Rechnung = [Posten]

>myrechnung::Rechnung
>myrechnung = [(1,"Ententopf-Leber-Terrine und gebratene Gaense-leber auf Apfel mit Balsamico-Essig",17.70),(2,"Flusskrebse in Estragongelee auf Safranschaum",12.50),(2,"Seeteufel im Tempurateig auf  Ingwergemuese  mit Baerlauchnudeln und Krebsen",25.30),(3,"Limonen-Quark- Mousse auf Brombeermark mit   Mangostreifen",10.90),(2,"Frische Feigen in Rotwein pochiert mit weissem Portweinschaum",6.90),(10,"Moet & Chandon Flasche 0.75l",59.10),(123,"Apfel",0.39)]

>dieRechnungBitte::IO()
>dieRechnungBitte = rechnung(myrechnung)

Testlauf:
---------

Main> dieRechnungBitte



* * * * * * * * * * * * * * * * * * * * * *
*                                         *
*          Gasthof zum ALPtraum           *
*         Inhaber:  B.J. und C.M.         *
*                                         *
* * * * * * * * * * * * * * * * * * * * * *
* Rechnung:                      28.11.02 *
*                                         *
* Anz.  Speise           Einzel   Summe   *
* --------------------------------------- *
*    1  Ententopf-Leber                   *
*       -Terrine und ge                   *
*       bratene Gaense-                   *
*       leber auf Apfel   17.70   17.70   *
*    2  Flusskrebse in                    *
*       Estragongelee a                   *
*       uf Safranschaum   12.50   25.00   *
*    2  Seeteufel im Te                   *
*       mpurateig auf                     *
*       Ingwergemuese                     *
*       mit Baerlauchnu   25.30   50.60   *
*    3  Limonen-Quark-                    *
*       Mousse auf Brom                   *
*       beermark mit                      *
*       Mangostreifen     10.90   32.70   *
*    2  Frische Feigen                    *
*       in Rotwein poch                   *
*       iert mit weisse                   *
*       m Portweinschau    6.90   13.80   *
*   10  Moet & Chandon                    *
*       Flasche 0.75l     59.10  591.00   *
*  123  Apfel              0.39   47.97   *
* --------------------------------------- *
* Gesamtsumme:                   778.77   *
* zzgl. 16% Mwst.                903.38   *
*                                ======   *
* (alle Preise in Euro)                   *
* * * * * * * * * * * * * * * * * * * * * *
* Vielen Dank fuer Ihren Besuch!          *
* * * * * * * * * * * * * * * * * * * * * *


