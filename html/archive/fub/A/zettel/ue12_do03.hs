-- ALP1 Uebung 12
-- Alexander Blotny, Stefan Kadereit
-- Tutor: Till Zoppke

-- Aufgabe 1

data List a = Leer|Cons (List a) a
	deriving (Eq,Ord,Show)

-- Testliste
test::List Int
test = Cons (Cons (Cons (Cons Leer 4) 2) 5) 3

cat::List a->List a->List a
cat Leer ys = ys -- falls xs Leer
cat xs Leer = xs -- Rekursionsanker
cat xs (Cons ys y) = Cons (cat xs ys) y -- rekursiver Aufruf von Cat

ind::Int->List a -> a
ind n _|n<=0 = error "kein Element" -- falsche Eingabe
ind n (Cons xs x)
 |n > mlength (Cons xs x) = error "geht nich" -- falls n > Länge der Liste
 |n == mlength (Cons xs x) = x -- Rekursionsanker wenn Länge der Liste = n
 |otherwise = ind n xs  -- rekursiver Aufruf

mlength::List a->Int
mlength Leer = 0 -- Rekursionsanker
mlength (Cons xs x) = 1 + mlength xs -- rekursiver Aufruf von mlength

mhead::List a ->a
mhead Leer = error "leere Liste" -- leere Liste hat keinen head
mhead (Cons Leer x) = x -- Rekursionsanker
mhead (Cons xs _) = mhead xs -- rekursiver Aufruf

mtake::Int->List a->List a
mtake n _|n<=0 = error "geht nich" -- falsche Eingabe
mtake n (Cons xs x)
 |mlength (Cons xs x) <= n = (Cons xs x) -- Rekursionsanker Liste so lang wie n
 |otherwise = mtake n xs -- rekursiver Aufruf

mmap::(a->b) -> List a -> List b
mmap _ Leer = Leer -- Rekursionsanker
mmap f (Cons xs x) = Cons (mmap f xs) (f x) -- rekursiver Aufruf mit Anwendung der Funktion

-- Aufgabe 2
-- Definition von Baum
data Baum a = Leerb |Knoten a [Baum a]
	deriving(Eq,Ord)

testbaum::Num a=>Baum a
testbaum = Knoten 1 [Knoten 2 [Knoten 8 [Leerb]],Knoten 3 [Knoten 4 [Knoten 5 [Leerb],Knoten 6 [Leerb]],Knoten 7 [Leerb]]]

t2 = Knoten "Elizabeth" 
                        [ Knoten "Charles" 
                                         [Knoten "William" [],
                                          Knoten "Henry"   []
                                         ],                 

                          Knoten "Andrew" 
                                        [Knoten "Beatrice"  [],
                                          Knoten "Eugenie"   []
                                         ],
                          Knoten "Edward" [],
                          Knoten "Anne"
                                         [Knoten "Peter"  [],
                                          Knoten "Zara"   []
                                         ] 
                         ]

--a)
wurzel::Baum a -> a
wurzel Leerb = error "keine Wurzel"
wurzel (Knoten x _) = x -- Wurzel ist erstes Element von Baum

--b)
finde::Eq a=> a->Baum a -> Bool
finde _ Leerb = False -- im Leerb ist kein Element
finde x (Knoten y ys)
 |x==y = True -- Element is enthalten
 |otherwise = or (map (finde x) ys) -- Verknüpfung aller Bäume mit finde (or faßt alle zusammen)

--c)
vater::Eq a=> a->Baum a -> a
vater _ Leerb = error "nix da" -- kein Vater
vater x (Knoten z ys)
 |gleich x ys = z -- x wird mit Baumliste verglichen, falls enthalten ist z Vater
 |otherwise = help x ys -- help wird aufgerufen wenn Vater noch nicht gefunden

help::Eq a=>a->[Baum a] -> a
help _ [] = error "kein Vater mit solchem kind" -- es existiert kein Vater
help x (y:ys)
 |finde x y = vater x y -- Baumstruktur wird in Richtung des gesuchten Kindes durchlaufen
 |otherwise = help x ys -- Liste wird weiter nach dem Kind durchsucht

gleich::Eq a=>a->[Baum a]->Bool
gleich x [] = False -- Abbruch wenn Liste durchlaufen und kein Kind gefunden
gleich x ((Knoten z zs):ys)
 |z==x = True -- Kind gefunden
 |otherwise = gleich x ys -- rekursiver Aufruf

--d)
kinder::Eq a=>a->Baum a->[a]
kinder x (Knoten z ys)
 |(x==z) = ausgabe ys -- Vater gefunden -> Ausgabe
 |otherwise = help2 x ys -- Aufruf von help2

help2::Eq a=>a->[Baum a]->[a] 
help2 _ [] = error "keine Kinder mit solchem Vater"
help2 x (y:ys)
 |finde x y = kinder x y -- Struktur wird dahin durchsucht wo sich das Kind befindet
 |otherwise = help2 x ys -- Liste wird bei weiter durchsucht

ausgabe::[Baum a]->[a]
ausgabe [] =[] -- Rekursionsanker
ausgabe [Leerb] = error "keine Kinder mit solchem Vater"
ausgabe ((Knoten y zs):ys) = y:(ausgabe ys) -- Kinder werden in eine Liste erstellt

--e)
tiefe::Eq a=>a->Baum a->Int
tiefe _ Leerb = error "geht nich" -- geht nicht auf Leerb
tiefe x (Knoten z ys)
 |(x==z) = 0 -- Rekursionsanker
 |otherwise = help3 x ys -- Aufruf help3

help3::Eq a=>a->[Baum a]->Int
help3 _ [] = error "geht nich" -- wenn Element nicht enthalten
help3 x (y:ys)
 |finde x y = 1 + tiefe x y -- Element in Struktur 1 addieren und rek. aufrufen
 |otherwise = help3 x ys -- Element weitersuchen

-- Aufgabe 3
-- Instanz Show
instance Show a=>Show (Baum a) where
	show x = printbaum 0 x -- 0 als Startwert

printbaum::Show a=>Int-> Baum a -> String
printbaum anz (Knoten x ys) = (striche (anz)) ++ show (x) ++ "\n" ++ (teile (anz+1) ys)
	-- gibt einen Knoten mit bestimmter Anzahl Striche aus und ruft teile mit anz+1 auf

teile::Show a=>Int->[Baum a]->String
teile _ [] =[] -- Rekursionsanker
teile _ [Leerb] = [] -- Fall Leerb
teile anz (y:ys) = (printbaum anz y) ++ (teile anz ys)
	-- neuer Knoten wird erstellt und dessen Kinder angehangen...

-- nur zur Erstellung der Striche
striche::Int->String
striche 0 = []
striche n = (replicate (n*5) '-') ++ "|"

{- TESTLÄUFE

Main> cat test test
Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Leer 4) 2) 5) 3) 4) 2) 5) 3
Main> ind 5 (cat test test)
4
Main> mlength test
4
Main> mhead test
4
Main> mtake 2 test
Cons (Cons Leer 4) 2
Main> mmap (\x->x^2) test
Cons (Cons (Cons (Cons Leer 16) 4) 25) 9
Main> wurzel t2
"Elizabeth"
Main> finde "Till" t2
False
Main> vater "Charles" t2
"Elizabeth"
Main> kinder "Elizabeth" t2
["Charles","Andrew","Edward","Anne"]
Main> tiefe "William" t2
2
Main> putStr(show t2)
"Elizabeth"
-----|"Charles"
----------|"William"
----------|"Henry"
-----|"Andrew"
----------|"Beatrice"
----------|"Eugenie"
-----|"Edward"
-----|"Anne"
----------|"Peter"
----------|"Zara"
-}   