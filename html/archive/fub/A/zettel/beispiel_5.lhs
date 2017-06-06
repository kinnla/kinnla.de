---------------- Hinweise zum Übungsblatt 5, Aufgaben 2, 3 ------------------------

------------------------------------- Aufgabe 2 -----------------------------------

 
a) und b) ähneln einander. Man sollte sich (immer) darüber Gedanken machen,
was bei der Eingabe von 0 oder von negativen Zahlen passiert. Resultiert daraus
ein Fehler? Dann muss man ihn abfangen. Wieviele Elemente hat die Liste [1..0]?

Bei Funktionen, mit denen man Sachverhalte aus der Mathematik nachbildet, sollte
man im Zweifelsfall aufschreiben, wie dieser Sachverhalt in der Mathematik 
definiert ist. 

Eine mögliche Definition von a | b (a ist Teiler von b): 
a | b, falls a/=0 und b mod a == 0. Sonst nicht.
Nach dieser Definition erhalten wir für jede Int-Zahl eine Aussage. 
Das entsprechende Haskell Programm sieht so aus:

> (!!!) :: Int -> Int -> Bool
> a !!! b = a /= 0 && b `mod` a == 0

Eine andere mögliche Definition von a | b:
falls a>0 und b>0 und b mod a == 0, dann a | b
falls a>0 und b>0, aber b mod a /= 0, dann ist a kein Teiler von b
falls a<=0 und b<=0, dann nicht definiert.
Nach dieser Definition erhalten wir nur für positive Zahlen eine Aussage,
ansonsten ist das Ergebnis undefiniert. Das entsprechende Haskell Programm
muss den undefinierten Fall abfangen und eine Fehlermeldung ausgeben, zB:

> (!!!!) :: Int -> Int -> Bool
> a !!!! b
>  | a > 0 && b > 0   = b `mod` a == 0
>  | otherwise        = error "nur fuer positive Zahlen definiert"

Ein Beispiel für ein einfaches Listenprogramm:

> -- gibt alle Vielfachen von k aus, die kleiner als n sind.
> -- Anmerkung: bei k<=0 und n>0 gäbe es eine unsinnige Ausgabe von n Zahlen
> vielfache :: Int -> Int -> [Int]
> vielfache k n 
>  | k > 0       = [i * k | i <- [1..n], i * k <= n]
>  | otherwise   = error "nur für positive k definiert"

Wenn man die Funktion "teiler" definiert hat, ist es eine nette Spielerei, sich
eine Funktion "istPrim" zu definieren, welche "teiler" verwendet:

> teiler :: Int -> [Int]
> teiler = ... selber implementieren

> istPrim :: Int -> Bool
> istPrim n = teiler n == [1,n]

> istPrim2 :: Int -> Bool
> istPrim2 n = length (teiler n) == 2

Wieso funktioniert das? Nun -- jede Zahl ist durch sich selbst und durch 1 teilbar.
Also ergeben sich für eine Primzahl genau 2 Teiler. Bei Eingabe von n=1 erhalten 
wir eine Liste der Länge 1, also False. Das ist korrekt, denn 1 ist keine Primzahl.
Bei 0 oder negativen Eingaben hängt das Ergebnis davon ab, wie wir diese Fälle bei
"teiler" definiert haben.

In diesem Zusammenhang ist es interessant, einmal die Einstellung "print statistics"
von Haskell auszuprobieren. man schaltet diese mit dem Kommando ":s +s" ein, und mit
dem Kommando ":s -s" wieder aus. So kann man zu jedem Funktionsaufruf die Anzahl
der Auswertungen und Speicherzellen sehen, die Haskell vornimmt. Bei meiner 
Implementation von "teiler" ergibt sich folgendes:

Main> teiler 123456
[1,2,3,4,6,8,12,16,24,32,48,64,96,192,643,1286,1929,2572,3858,5144,7716,10288,15
432,20576,30864,41152,61728,123456]
(3086669 reductions, 3704226 cells, 15 garbage collections)
Main> istPrim 123456
False
(91 reductions, 149 cells)
Main> istPrim2 123456
False
(3086650 reductions, 3704048 cells, 15 garbage collections)
Main>

Dass "istPrim" soviel schneller ist als "teiler", liegt an der sogenannten 
"lazy evaluation" in Haskell. Ausdrücke werden dann nicht weiter ausgewertet,
wenn das Ergebnis schon klar ist. Man sieht ja schon am Aufruf von "teiler", dass
die Liste nicht als Ganzes ausgegeben wird, sondern Zahl für Zahl. Somit ist 
bereits mit dem zweiten Listenelement klar, ob eine Primzahl vorliegt oder nicht.
Bei "istPrim2" funktioniert das nicht, da die Liste fertig sein muss, bevor auf
sie die Funktion "length" angewendet werden kann. Z.B. mit "length [1..]" schickt 
man Haskell in eine Endlosschleife, obwohl doch eigentlich klar ist, dass die Länge 
unendlich ist.

Bei Aufgabe c) ist die Eingabe eine Liste von Listen eines beliebigen Typs.
Kritische Eingaben sind hier "[]", "[[]]", "[[2],[]". Falls Haskell bei eurer
Lösung folgende Fehlermeldung ausgibt, so ist das KEIN Programmfehler:

Main> anf [[]]
ERROR - Cannot find "show" function for:
*** Expression : anf [[]]
*** Of type    : [a]

Haskells Problem ist, dass der Typ der Funktionsdefinition allgemein gehalten ist.
Bei einer konkreten Eingabe (z.B. "[[3]]") ist klar, um welchen Typ es sich handelt,
und ein Ergebnis kann angezeigt werden. bei Eingabe von "[[]]" ist nicht klar,
um eine leere Liste welchen Typs es sich handelt, der Typ ist weiterhin allgemein.
Für einen allgemeinen Typ gibt es keine Möglichkeit der Ausgabe, und so muss 
Haskell passen. Einen Fehler bei der Eingabe "[[3][]]" solltet ihr hingegen bitte
abfangen.

Hier noch ein einfaches Beispiel für den Umgang mit Listen von Listen:

> -- Erzeugt aus einer Liste von Listen eine einzige Liste, die alle Elemente umfasst.
> -- vergleiche vordefinierte Funktion "concat" aus dem prelude.
> konkat :: [[a]] -> [a]
> konkat l = [i | j <- l, i <- j]

------------------------------------ Aufgabe 3 ------------------------------------

Für Aufgabe 3 wird ein Schönheitspreis vergeben.
Ein ähnliches Problem wird in Thompson, S. 108 besprochen.
Bevor man sich ans Programmieren macht, ist es bei komplexen Problemen hilfreich,
sich eine Struktur für das Programm zu überlegen, einen sogenannten "Entwurf"
anzufertigen. Hier ein möglicher Entwurf:

> -- Datentypen, in denen wir unsere Informationen halten
> type Posten = (Int, String, Float)
> type Rechnung = [Posten]

> -- Unsere konkrete Rechnung, damit wir nicht alles neu eingeben müssen
> myrechnung :: Rechnung
> myrechnung = [(5, "Koepi", 2.20), (1, "Pommes", 1.80)]

> -- erzeugt aus einem Posten eine Zeile einer bestimmten Länge.
> makeLine :: Posten -> Int -> String

> -- bringt eine Rechnung auf den Bildschirm
> printRechnung :: Rechnung -> IO()

> -- erzeugt aus einer Rechnung einen String
> -- wird von printRechnung benutzt
> makeRechnung :: Rechnung -> String

> -- gibt myRechnung aus
> -- diese Funktion bitte in jeder Lösung definieren!
> dieRechnungBitte :: IO()

Was noch zu tun ist, ist das bilden der Summe, das Ausrechnen der Mehrwertsteuer, 
die Formatierung der Ausgabe. Einen Zeilenumbruch erhält man mit "\n".

einige hilfreiche, bereits vordefinierte Funktionen (vgl. Prelude):

> -- gibt einen String auf dem Bildschirm aus
> putStr :: String -> IO()

> -- Wandelt Zahlen in Strings um
> show :: Float -> String
> show :: Int -> String

> -- hängt Strings aneinander
> (++) :: String -> String -> String

noch ein Beispielprogramm für Typdeklarationen und Bildschirmausgabe

> -- Typdeklaration
> type Name = String
> type Alter = Int

> -- Liste von Namen
> namen :: [Name]
> namen = ["Bert", "Franka"]

> -- Liste von Altern
> alter :: [Alter]
> alter = [23, 25]

> -- erzeugt eine Liste von Namen mit zugehörigem Alter als String
> -- falls die Listen unterschiedlich lang sind, werden dummy Werte ausgegeben
> makeListe :: [Name] -> [Alter] -> String
> makeListe [] []         = ""
> makeListe (n:ns) []     = makeListe (n:ns) [-1]
> makeListe [] (a:as)     = makeListe ["no name"] (a:as)
> makeListe (n:ns) (a:as) = n ++ " ist " ++ show a 
>                           ++ " Jahre alt.\n" ++ makeListe ns as

> -- gibt eine Liste auf dem Bildschirm aus
> printListe :: [Name] -> [Alter] -> IO()
> printListe a b = putStr (makeListe a b)

