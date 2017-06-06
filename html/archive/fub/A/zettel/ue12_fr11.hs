--Altansumiya Gombojav, MieRie Lim-Becker
--Uebung 12
--2. Aufgabe
--2.a 

data Baum a = Leer | Knoten a [Baum a]
  	        deriving (Eq)

wurzel:: Baum a -> a
wurzel Leer = error "Baum ist leer"
wurzel (Knoten v _) = v

{- Main> wurzel t3
"Elizabeth"      -}

--2.b 

finde:: Eq a => a -> Baum a -> Bool
finde e Leer = False
finde e (Knoten v l) = foldr (||) (e==v) (map (finde e) l)

{- Main> finde "Elizabeth" t3
True 
Main> finde "Sumiya" t3
True
Main> finde "MiRie" t3
False
Main> finde "" t3
False          -}

--2.c

vater :: Eq a => a -> Baum a -> a
vater e Leer = error "Baum ist Leer"
vater e v 
	|length v1 == 0= error "das Element im Baum nicht vorhanden"
	|otherwise = head v1 
		where v1 = vater1 e v

vater1 :: Eq a => a -> Baum a -> [a]
vater1 e Leer = []
vater1 e (Knoten v l) 
	|v1 = [v] 
	|otherwise = concat (map (vater1 e) l)
	  where 
	    v1 = foldr (||) False (map (==[e]) (map w1 (getBaum (Knoten v l)))) 

{- Main> vater "Sumiya" t3
"Charles"
Main> vater "Sumiya" Leer
"
Program error: Baum ist Leer

Main> vater "" t3
"
Program error: das Element im Baum nicht vorhanden             -}

--2.d

kinder :: Eq a => a -> Baum a -> [a]
kinder e Leer = []
kinder e (Knoten v l)
	|e==v = concat (map w1 l)
	|otherwise = concat (map (kinder e) l)

{- Main> kinder "Sumiya" t3
[]
Main> kinder "Charles" t3
["Sumiya","William","Henry"]       
Main> kinder "ACharles" t3
[]     -}

--2.e

tiefe :: Eq a => a -> Baum a -> Int
tiefe _ Leer = error "Baum ist leer2"
tiefe e (Knoten v l) 
	|e==v = 0
	|t1 /= 0 = t1
	|otherwise = error "diese Beschriftung ist im Baum nicht vorhanden"
		where t1 = tiefe1 0 e (Knoten v l)

tiefe1 :: Eq a => Int -> a -> Baum a -> Int
tiefe1 _ _ Leer = 0
tiefe1 anz e (Knoten v l) 
	|e==v = anz
	|otherwise = sum (map (tiefe1 (anz+1) e) l )
{-Main> tiefe "Z1-I" t5
5
Main> tiefe "Elizabeth" t5
0
Main> tiefe "AElizabeth" t5

Program error: diese Beschriftung ist im Baum nicht vorhanden 
Main> tiefe "" t3

Program error: diese Beschriftung ist im Baum nicht vorhanden
Main> tiefe "Sumiya" Leer

Program error: Baum ist leer2                   -}

                                                               
--2.f

cousins :: Eq a => Int -> a -> Baum a -> [a]
cousins n x v 
	|n<(-1) = error "falsche Argument: Grad soll => -1"
	|n > hoehe-1 = error "der Tiefe von x ist niedriger als das Argument n: "
	|n == (-1) = [x] 
	|otherwise = filter (/=x) (nachbB (n+1) (vorgB (hoehe-n-1) x [v]))
		where 
			hoehe = tiefe x v

{-Main> cousins 1 "Z1-I" t5
["Z1-II","Z2-I","Z2-II"]
Main> cousins 2 "Z1-I" t5
["O1-I","Z1-II","Z2-I","Z2-II"]
Main> cousins 3 "Z1-I" t5
["O1-I","Z1-II","Z2-I","Z2-II","U1-I","H1-II"]
Main> cousins 4 "Z1-I" t5
["O1-I","Z1-II","Z2-I","Z2-II","U1-I","H1-II","P1-II"]
Main> cousins 5 "Z1-I" t5

Program error: der Tiefe von x ist niedriger als das Argument n:      -}

nachbB :: Int -> [Baum a] -> [a]
nachbB n [] = []
nachbB n l 
	|n== 0 = concat (map w1 l)
	|otherwise = nachbB (n-1) (concat (map getBaum l))

vorgB :: Eq a => Int -> a -> [Baum a] -> [Baum a]
vorgB  n x [] = []
vorgB n x l 
	|n== 0 = filter (finde x) l
	|otherwise = vorgB (n-1) x fList
		where fList = filter (finde x ) (concat (map getBaum l))

w1:: Baum a -> [a]
w1 Leer = []
w1 (Knoten v _) = [v]

getBaum:: Baum a -> [Baum a]
getBaum Leer = []
getBaum (Knoten x bs)   = bs

--3. Aufgabe

instance Show a => Show (Baum a) where
    show Leer = "[]"
    show (Knoten v l) = showTree 0 "" (Knoten v l) 
	where 
	  showTree acc x Leer = []
	  showTree acc x (Knoten v l) = (concat (map (showTree (acc+1) "/- ") l1))++
	   (space acc)++x++(show v)++" -- "++"\n"++(concat (map (showTree (acc+1) h) l2))
		 where 
			baum = getBaum (Knoten v l)
			half = (length baum)`div`2
			l1 = take half baum
			l2 = drop half baum
			h = [(toEnum 92)]++"- " 

{-
Main> putStr (show t5)
                                                            /- "O1" --
                                                                           \- "O1-I" --
                                             /- "Orgil" --
                                                            \- "O2" --
                              /- "Sumiya" --
                                                                           /- "Z1-I" --
                                                            /- "Z1" --
                                                                           \- "Z1-II" --
                                             \- "Zorig" --
                                                                           /- "Z2-I" --
                                                            \- "Z2" --
                                                                           \- "Z2-II" --
               /- "Charles" --
                                             /- "Unbekannt" --
                                                            \- "U1" --
                                                                           \- "U1-I" --
                              \- "William" --
                                             \- "W1" --
                                             \- "W2" --
                              \- "Henry" --
                                             \- "H1" --
                                                            \- "H1-I" --
                                                                           \- "H1-II" --
                              /- "Beatrice" --
               /- "Andrew" --
                              \- "Eugenie" --
"Elizabeth" --
               \- "Edward" --
                                             /- "P1" --
                                                            \- "P1-I" --
                                                                           \- "P1-II" --
                              /- "Peter" --
                                             \- "P2" --
               \- "Anne" --
                              \- "Zara" --
                                                                                      
Main>     -}


dist :: Int
dist = 15

space::Int->String
space 0 =[]
space n = replicate (dist*n) ' '

-- Beispiele:
-- ------------
t2 = Knoten "Elizabeth" [ Knoten "Charles" [Knoten "William" [],Knoten "Henry" []], 
     Knoten "Andrew" [Knoten "Beatrice" [],Knoten "Eugenie" []],Knoten "Edward" [],
     Knoten "Anne"[Knoten "Peter"  [],Knoten "Zara"   []]] 
t3 = Knoten "Elizabeth" [Knoten "Charles" [Knoten "Sumiya" [Leer], 
     Knoten "William" [Leer], Knoten "Henry" [Leer]], Knoten "Andrew" 
     [Knoten "Beatrice" [Leer], Knoten "Eugenie" [Leer]],Knoten "Edward" [Leer],
     Knoten "Anne"[Knoten "Peter"  [Leer],Knoten "Zara"   [Leer]]]
t4 = Knoten "Elizabeth" [ Knoten "Charles" [Knoten "Sumiya" [Knoten "S1" [Leer], 
     Knoten "S2" [Leer]], Knoten "William" [Knoten "Unbekannt" [Leer],
     Knoten "W1" [Leer], Knoten "W2" [Leer]],Knoten "Henry" [Leer]], 
     Knoten "Andrew" [Knoten "Beatrice" [Leer],Knoten "Eugenie" [Leer]],
     Knoten "Edward" [Leer],Knoten "Anne"[Knoten "Peter" [Leer],Knoten "Zara" [Leer]]]
t5 = Knoten "Elizabeth" [ Knoten "Charles" [Knoten "Sumiya" [Knoten "Orgil" 
     [Knoten "O1" [Knoten "O1-I" [Leer]], Knoten "O2" [Leer]], Knoten "Zorig" 
     [Knoten "Z1" [Knoten "Z1-I" [Leer], Knoten "Z1-II" [Leer]],Knoten "Z2" 
     [Knoten "Z2-I" [Leer], Knoten "Z2-II" [Leer]]]], Knoten "William" 
     [Knoten "Unbekannt" [Knoten "U1" [Knoten "U1-I" [Leer]]],Knoten "W1" [Leer], 
     Knoten "W2" [Leer]],Knoten "Henry" [Knoten "H1" [Knoten "H1-I" 
     [Knoten "H1-II" [Leer]]]]], Knoten "Andrew" [Knoten "Beatrice"  [Leer],
     Knoten "Eugenie"   [Leer]],Knoten "Edward" [Leer],Knoten "Anne"[Knoten "Peter" 
     [Knoten "P1" [Knoten "P1-I" [Knoten "P1-II" [Leer]]], Knoten "P2" [Leer]],
     Knoten "Zara" [Leer]]]


--1. Aufgabe

data Liste a = Nil | Cons (Liste a) a
			deriving (Eq, Ord, Read)

instance (Show a ) => Show (Liste a) where
	show Nil = []
	show (Cons l e) = (showL l)++";"++show e++"]"
		where 
			showL (Cons Nil e) = "["++show e
			showL (Cons l e) = (showL l)++";"++show e

cat :: (Liste a) -> (Liste a) -> (Liste a) 
cat Nil Nil = Nil
cat Nil c = c
cat c Nil = c
cat l (Cons l1 e1) = Cons (cat l l1 ) e1

mlength :: (Liste a) -> Int
mlength Nil = 0
mlength (Cons l e) = 1 + mlength l

ind :: Int -> (Liste a) -> a
ind n Nil= error "Index too large"
ind n (Cons l e) 
	|n<0 = error "negative Index"
	|n>ml-1 = error "Index too large"	
	|otherwise = ind1 (ml - n-1) (Cons l e)
		where 	ml = mlength (Cons l e) 
			ind1 n (Cons l e)
				|n==0 = e
				|otherwise = ind1 (n -1) l

mhead :: (Liste a) -> a
mhead Nil = error "empty list"
mhead l = ind 0 l

mtake :: Int -> (Liste a) -> (Liste a)
mtake 0 _ = Nil
mtake _ Nil = Nil
mtake n (Cons l e) 
	|n>ml-1 = error "index too large"
	|otherwise = mtake1 (ml - n) (Cons l e) 
		where 	ml = mlength (Cons l e) 
			mtake1 n (Cons l e)
				|n==0 = Cons l e
				|otherwise = mtake1 (n -1) l


mmap :: (a -> a) -> (Liste a) -> (Liste a)
mmap f Nil = Nil
mmap f (Cons l e) = (Cons (mmap f l) (f e))

{- FUNKTION cat:

Main> cat (Cons (Cons (Cons (Cons Nil "b") "c") "e") "d") (Cons Nil "f")
["b";"c";"e";"d";"f"]                 
Main> cat (Cons (Cons (Cons (Cons Nil 'h') 'a') 'l') 'l') (Cons Nil 'o')
['h';'a';'l';'l';'o']                 
Main> cat (Cons (Cons (Cons (Cons Nil 1) 2) 3) 4) (Cons Nil 5)
[1;2;3;4;5]              

FUNKTION ind:
Main> ind 3 (Cons (Cons (Cons (Cons Nil "b") "c") "e") "d")
"d"     
Main> ind 3 (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd')
'd'
Main> ind 3 (Cons (Cons (Cons (Cons Nil 1) 2) 3) 4)
4                                                           
Main> ind 4 (Cons (Cons (Cons (Cons Nil "b") "c") "e") "d")
"
Program error: Index too large      

Main> ind (-1) (Cons (Cons (Cons (Cons Nil "b") "c") "e") "d")
"
Program error: negative Index                      

FUNKTION mhead:
Main> mhead (Cons (Cons (Cons (Cons Nil 1) 2) 3) 4)
1                        

FUNKTION mtake:
Main> mtake 1 (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
['b'
Main> mtake 0 (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
[]
Main> mtake 1 (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
['b'
Main> mtake 2 (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
['b';'c']
Main> mtake 3 (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
['b';'c';'e']
Main> mtake 4 (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
['b';'c';'e';'d']                  


Main> mmap (+2) (Cons (Cons (Cons (Cons (Cons Nil 1) 2) 3) 4) 5)
[3;4;5;6;7]         
Main> mmap (+2) (Nil)
[]        

Main> show (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
"['b';'c';'e';'d';'f']"        
-}


{- WIESO??
Main> mhead (Leer)
ERROR - Cannot find "show" function for:
*** Expression : mhead Leer
*** Of type    : a

Main> head []
ERROR - Cannot find "show" function for:
*** Expression : head []
*** Of type    : a      

Main> mtake 1 (Cons (Cons (Cons (Cons (Cons Nil 'b') 'c') 'e') 'd') 'f')
['b'
-}

