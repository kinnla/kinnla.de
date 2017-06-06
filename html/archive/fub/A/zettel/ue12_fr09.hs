--Aufgabe 1:
------------

data Liste a = Leer | Cons (Liste a) a
	deriving (Eq,Ord,Show)
	
		
cat :: (Liste a) -> (Liste a) -> (Liste a)
cat Leer (Cons xs x) = Cons xs x
cat (Cons xs x) Leer = Cons xs x
cat xs (Cons ys y)    = Cons (cat xs ys) y
						
-- Ist eine der beiden Listen leer, so wird die andere Liste ausgegeben
-- Ansonsten werden die beiden Listen rekursiv aneinander gehängt


ind :: Int -> (Liste a) -> a
ind _ Leer = error "fehlerhafte Eingabe"
ind n (Cons xs x)
	  | n >  ( mlength (Cons xs x) ) = error "es exestiert kein n-tes Element (d.h. n ist größer als die Länge der Liste)"
	  | n <= 0			 = error "es exestiert kein 0-tes Element bzw. negative Elemente" 
	  | n == ( mlength (Cons xs x) ) = x
   	  | otherwise 			 = ind n xs

-- Fehler werden abgefangen
-- Ansonsten wird rekursiv das letzte Element der Liste entfernt, bis man zum gewünschten Element gelangt.	
	
	
mlength :: (Liste a) -> Int
mlength Leer = 0
mlength (Cons xs x) = 1+(mlength (xs))

-- Die Leere Liste hat die Länge 0
-- Erneut rekursiver Aufruf, bei jedem Durchlauf wird um 1 hochgezählt


mhead :: (Liste a) -> a
mhead Leer = error "Die leere Liste hat keinen Kopf"
mhead (Cons Leer x) 			  = x
mhead (Cons xs x) 
	   | 0 == ( mlength (Cons xs x) ) = x
	   | otherwise 		          = mhead xs
	   
-- Die leere Liste hat keinen Kopf
-- Bei der Liste mit nur einem Element, ist dies gleich dem Kopf der Liste (die Leere Liste ist Präfix)
-- Ansonsten wird rekursiv die Liste aufgerufen und bei jedem Aufruf, das letzt Element entfernt,bis man zum Kopf der Liste 
-- kommt. Analog ginge es mit dem Aufruf: ind 1 (Liste a)


mtake :: Int -> (Liste a) -> (Liste a)
mtake _ Leer = Leer
mtake n (Cons xs x)
	     | n >= ( mlength (Cons xs x)  ) = Cons xs x
	     | n == ( mlength (Cons xs x)  ) = Cons xs x
      	     | otherwise		     = mtake n xs

-- Aus der leeren Liste kann man nichts nehmen, sie ist leer
-- Wenn man mehr Elemente aus der Liste nehmen möchte, als sie Elemente besitzt wird die gesamte Liste ausgegeben
-- Ansonsten wird wieder rekursiv die Liste aufgerufen, solange bis die Liste die Länge hat, die man haben möchte.
-- Analog zur Funktion ind, nur das man diesmal die Restliste ausgibt.


mmap :: (a -> b) -> (Liste a) -> (Liste b)
mmap f Leer = Leer
mmap f (Cons xs x) = cat (mmap f xs) (Cons Leer (f x))

-- mmap wendet eine Funktion auf jedes Element der Liste an. Dies geschieht indem die Funktion auf die Elemente der Liste
-- angwendet wird. Dabei wird rekursiv von hinten nach vorne durchgegangen. Danach fügt die Funktion cat sie zur neuen Liste 
-- zusammen

----------------------------------------------------------------------------
-- Listen zum Testen

testa :: (Liste Int)
testa = (Cons (Cons (Cons (Cons (Leer) 1) 2 ) 3 ) 4)

testa1 :: (Liste Int)
testa1 = (Cons (Cons (Cons (Cons (Leer) 5) 6 ) 7 ) 8)

testb :: (Liste Char)
testb = (Cons (Cons (Cons (Cons (Leer) '1') '2' ) '3' ) '4')

testb1 :: (Liste Char)
testb1 = (Cons (Cons (Cons (Cons (Leer) '5') '6' ) '7' ) '8')

testc :: (Liste String)
testc = (Cons (Cons (Cons (Cons (Leer) "Eins") "Zwei" ) "Drei" ) "Vier")

testc1 :: (Liste String)
testc1 = (Cons (Cons (Cons (Cons (Leer) "Fünf") "Sechs" ) "Sieben" ) "Acht")

{- TESTLÄUFE

Main> cat testa testa1
Cons (Cons (Cons (Cons (Cons (Cons (Cons (Cons Leer 1) 2) 3) 4) 5) 6) 7) 8
Main> ind 3 testc
"Drei"
Main> mlength testb
4
Main> mhead testa
1
Main> mtake 2 testb
Cons (Cons Leer '1') '2'     
Main> mmap (+10) testa
Cons (Cons (Cons (Cons Leer 11) 12) 13) 14 
Main> mmap (++"!") testc
Cons (Cons (Cons (Cons Leer "Eins!") "Zwei!") "Drei!") "Vier!"              
-}

----------------------------------------------------------------------------


--Aufgabe 2:
------------

data Baum a = LBaum | Knoten a [Baum a] 
	deriving (Eq)
	
wurzel :: Baum a -> a
wurzel LBaum = error " Der leere Baum hat keine Wurzel"
wurzel (Knoten x bs) = x 

-- Die Wurzel ist der erste Knoten in der Liste


finde :: (Eq a) => a -> Baum a -> Bool
finde n LBaum = False
finde n (Knoten x bs)
		| (n==x)	= True
		| otherwise     =  or (map(finde n) bs)

-- Im leeren Baum kann man nichts finden
-- Ansonsten ist entweder die Wurzel gleich der gesuchten Beschriftung, oder wir mappen die finde-Funktion in die Liste der 
-- Bäume und und falls ein "Treffer" dabei ist wertet sich der gesamte Ausdruck zur True aus, durch das logische ODER

	
vater :: (Eq a)=> a -> (Baum a) -> a
vater n LBaum 		= error "Der leere Baum hat keinen Vater"
vater n (Knoten x [])   = error "Es exestieren keine Kinder, also auch kein Vater"
vater n (Knoten x bs) 
       | (n==x)				= error "Die Wurzel eines Baumes hat keinen Vater"
       | finde n (Knoten x bs) == False = error "solch eine Beschrifung gibt es nicht in diesem Baum"
       | (any (n==) (kind)) 		= x
       | otherwise 			= head [ (vater n cs) | cs<-bs, (finde n cs) ]
           					where kind = (map name bs)
                				      name (Knoten cs us) = cs 
                				      
-- Falsche Eingaben werden abgefangen   
-- mit head bekommen wir aus der Liste den Namen unter Verwendung der Funktion finde            				     
     		
     							     								
kinder :: (Eq a) => a -> Baum a -> [a]
kinder n LBaum = []
kinder n (Knoten x bs) 
		| (n==x)  = map(\(Knoten x bs) -> x) bs    
		| otherwise = concat (map (kinder n) bs) 
		
-- Der leere Baum hat keine Kinder
-- Erneut verwenden wir map um aus der Liste die Kinder herauszubekommen


tiefe :: (Eq a)=> a -> (Baum a) -> Int
tiefe n (Knoten x bs) 
       | finde n (Knoten x bs) == False = error "solch eine Beschrifung gibt es nicht in diesem Baum"
       | (n==x) = 0
       | ((vater n (Knoten x bs)) ==x) = 1
       | otherwise = 1+(tiefe (vater n (Knoten x bs)) (Knoten x bs))

-- falsche Eingaben werden abgefangen
-- Ein Baum der nur eine Wurzel hat, hat die Tiefe 0
-- Es wird rekursiv aufgerufen und bei jedem Durchlauf 1 addiert

queen = Knoten "Elizabeth" [Knoten "Charles" [Knoten "William" [], Knoten "Henry" []],Knoten "Andrew" [Knoten "Beatrice" [], Knoten "Eugenie" []],Knoten "Edward" [], Knoten "Anne"[Knoten "Peter"[],Knoten "Zara" []]]

{- TESTLÄUFE

Main> wurzel queen
"Elizabeth"
Main> finde "Henry" queen
True
Main> finde "Till" queen
False
Main> vater "Zara" queen
"Anne"
Main> kinder "Charles" queen
["William","Henry"]
Main> tiefe "Henry" queen
2
Main> tiefe "Elizabeht" queen

Program error: solch eine Beschrifung gibt es nicht in diesem Baum

Main> tiefe "Elizabeth" queen
0
Main>                    
-}

----------------------------------------------------------------------------

--Aufgabe3:
-----------

instance (Eq a, Show a) => Show (Baum a) where
      show (Knoten x bs) = zBaum (Knoten x bs)

-- Baum a zur instance von Show machen

zBaum :: (Eq a, Show a)  => (Baum a) -> String
zBaum (Knoten x bs) = (show x) ++ zuBaum bs 0

zuBaum :: (Eq a, Show a) => [Baum a] -> Int -> String          
zuBaum bs n = concat (["\n"++(concat(replicate n " |  "))++ " +--" ++ (show x)++ (zuBaum bs (n+1)) | (Knoten x bs) <- bs ])
                                
-- zBaum zeichnet den Baum (die Wurzel)
-- zuBaum zeichnet die Unterbäume rekursiv nacheinander und formatiert die Ausgabe

{- TESTLÄUFE
Main> putStr(zBaum queen)
"Elizabeth"
 +--"Charles"
 |   +--"William"
 |   +--"Henry"
 +--"Andrew"
 |   +--"Beatrice"
 |   +--"Eugenie"
 +--"Edward"
 +--"Anne"
 |   +--"Peter"
 |   +--"Zara"        
-}