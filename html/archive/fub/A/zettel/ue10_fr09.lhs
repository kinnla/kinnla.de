=========================================================
                       10.�bung ALP 1            
      		 Sebastian Quick / Ewa Kampa
          		 Gruppe fr9                                    
=========================================================

 Aufgabe 1
----------

geforderte Eigenschaften:

1. Assoziativit�t
2. Kommutativit�t
3. Absorbtion

(|+|) Vereinigung - max, (|*|) Durchschnitt - min

1a

> class (Eq a) => Verband a where
>	(|+|) :: a->a->a
>	(|*|) :: a->a->a
	
1b
	
das logische ODER und UND erf�llen die Eigenschaften, wie wir in RS gelernt haben.

> instance Verband Bool where
>	(|+|) = (||)
>	(|*|) = (&&)

+ und - erf�llen das Absorbtionsgesetz nicht, somit w�hlen wir max und min, da sie die 3 Axiome erf�llen.
	
> instance Verband Int where
>	(|+|) = max
>	(|*|) = min
	
> instance Verband Char where
>	(|+|) = max
>	(|*|) = min
	
1c	

Paare, sind Tupel, wobei die def. Operationen auf die Tupel komponentenweise angewendet werden.

> instance (Verband a, Verband b) => Verband (a,b) where
>	(|+|) (x,y) (x1,y1) = ((|+|) x x1 , (|+|) y y1)
>	(|*|) (x,y) (x1,y1) = ((|*|) x x1 , (|*|) y y1)
	

Aufgabe 2
---------

2a

die def. Operatoren sollen nun auf Listen angewendet werden.
(|+|) stellt die Vereinigung da, ++ "vereinigt" zwei Listen.
(|*|) stellt den Durchschnitt da, es werden alle Elemente die sowohl in xs als auch in ys sind in die neue Liste geschrieben.
entf ist die bereits von fr�her bekannte Funktion, die doppelte aus einer Liste entfernt.

> instance (Eq a)=> Verband [a] where
>	(|+|) xs ys = entf xs++ys
>	(|*|) xs ys = entf [z | z <-xs , elem z ys]
	
	
> entf :: (Eq a) =>[a]->[a]
> entf [] = []
> entf (x:xs) 
>	| elem x xs = entf xs
>	| otherwise = x: entf xs


2b

dies funktioniert nur dann, wenn man Ord [a] im Prelude auskommentiert
In Ord befinden sich : (<=),(<),(>=),(>),max,min
Es geht hierbei um Teilmengenrelationen:
(<=) bedeutet, da� die eine Menge in der anderen enthalten ist, bzw. die Menge selbst ist
(<)  bedeutet, da� die Menge echte Teilmenge der anderen Menge ist
analog f�r (>=) (>)
max stellt die Vereinigung der Mengen da
min den Durschnitt der Mengen


> instance (Eq a) => Ord [a] where
>	x <= y  = (length.entf)(max x y) == (length.entf) y
>     	x >= y  = (length.entf)(max x y) == (length.entf) x
>	x <  y  = ((length.entf)(max x y) == (length.entf) y) && (length x /= length y)
>	x >  y  = ((length.entf)(max x y) == (length.entf) x) && (length x /= length y)
>	max x y = entf (x++y)
>	min x y = entf [z | z <-x , elem z y]

erkl�rendes Beispiel:
x <= y : man vereinigt die beiden Mengen, entfernt die Doppelten und z�hlt dann die Anzahl der Elemente, ist diese Anzahl gleich der Anzahl der 2ten Menge, dann ist die Menge 1 in der Menge 2 enthalten.
x <  y : kann analog zu (<=) definiert werden mit der Zusatzbedingung, da� die beiden Mengen nicht gleich gross sein d�rfen. 
analog f�r die anderen F�lle
max, min sind wie in 2a definiert. 

Aufgabe 3
---------

3a

wie in der VL definiert

> class Visible a where
>	toString :: a -> String
>	size     :: a -> Int	

> instance Visible Int where
>	toString = binaer
>	size	 = length.binaer

toString gibt die Binaerzahl aus, size gibt die Anzahl der Stellen der Binaerzahl aus

binaer berechnet unter Verwendung von binaerrechnung aus gegebenem Int die Binaerzahl und gibt sie als String aus.
Die Berechnung erfolgt nach der in RS vorgestellten Methode.
Das Vorzeichen ist das h�chstwertige Bit, 1 bedeutet negative Zahl | 0 bedeutet positive Zahl  Bsp: 1111 = -7   0111 = +7

> binaer :: Int -> String
> binaer n
>	| n == 0    = "0"
>	| n <  0    = "1"++binaerrechnung (negate n)
>	| otherwise = "0"++binaerrechnung n
	
> binaerrechnung :: Int -> String
> binaerrechnung 0 = []
> binaerrechnung n = (binaerrechnung (div n 2))++(show(mod n 2))

3b

(*) soll dem UND entsprechen, was bei Bool (&&) ist
(+) soll XOR entsprechen, das ist bei Bool (/=)
(-) ist wie (+) nur das man ein Argument negieren muss
negate ist not
abs, signum weissen wir der Identit�t zu, andere Zuweisungen machen keinen Sinn
fromInteger, fromInt haben wir so def., da� alle positiven Zahlen TRUE sein sollen, alle anderen False (negative und die Null)


> instance Num Bool where
>	(+)         = (/=)
>	(-)	    = ((+).negate)
>	(*)         = (&&)
>	negate	    = not
>	abs         = id
>	signum      = id 
>	fromInteger = (>0) 
>	fromInt	    = (>0)

TESTL�UFE:
	
> a :: Int
> a = 5
> b :: Int
> b = 6	
	
> c :: Char
> c = 'c'
> d :: Char
> d = 'd'


Aufgabe 1:
----------	

Main> (|+|) True False
True

Main> (|+|) False False
False

Main> (|*|) False False
False

Main> (|*|) True False
False

  
Main> (|+|) a b
6

Main> (|*|) a b
5

Main> (|+|) c d
'd'

Main> (|*|) c d
'c'
  
Main> (|+|) (a,b) (a,b)
(5,6)

Main> (|*|) (a,b) (a,b)
(5,6)
         
	
Aufgabe 2:
----------

Main> (|+|) "hallo" "du"
"halodu"

Main> (|*|) "abcde" "bc"
"bc"

                     
Main> [4..6] <= [1..10]
True

Main> [1..10] <= [1..10]
True

Main> [1..10] < [1..10]
False

Main> max [4..6] [1..10]
[1,2,3,4,5,6,7,8,9,10]

Main> min [4..6] [1..10]
[4,5,6]
   	

Aufgabe 3:

Main> toString a
"101"

Main> toString b
"110"

Main> size b
3    

Main> (+) True False
True

Main> (*) True False
False

Main> abs False
False

Main> True && (fromInteger 5)
True

Main> True && (fromInteger 0)
False
                        
