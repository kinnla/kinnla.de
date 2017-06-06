{- ==============================================================
Alp 1 - WS 02/03 (Tutorium: Till Zoppke)              Do_12
Monika Budde  / Emrah Somay 
Uebung 12, Aufg. 2 

Wir benutzen die Materialien aus der Vorlesung v. 27.1.
Die Algorithmen sind z.T. leider nicht laufzeitoptimiert.

============================================================== -}
data Baum a = Leer                -- leerer Baum oder
      | Knoten a [Baum a]         -- Wurzel und Liste von Unterbäumen
         -- deriving (Eq): sinnvoll nur, wenn Eq a, sonst moeglich: 
         -- t = Knoten ord [], was bei t == t zu einem Laufzeitfehler fuehrt, 
         -- obwohl dies - wg deriving (Eq) - eigentlich nicht zu erwarten sein 
         -- sollte. Besser: Fehler beim Laden, wenn t definiert wird.
         -- Andererseits setzen einige Funktionen fuer Baeume Eq a nicht voraus.
         -- Daher ist es m.E. am besten, hier statt mit "deriving" mit "instance 
         -- (+Kontext)" zu arbeiten.

instance (Eq a) => Eq (Baum a) where                 -- fuer cousins benoetigt
   Leer == Leer = True
   (Knoten x bs) == (Knoten y cs) = x == y && bs == cs
   _ == _ = False
          
preorder :: Baum a -> [a]
preorder Leer            = []
preorder (Knoten x bs)   = x : concat (map preorder bs)

hoehe:: Baum a -> Int
hoehe Leer            = -1                                -- Konvention
hoehe (Knoten _ bs)   = 1 + foldr max (-1) (map hoehe bs) -- Max. der Hoehen der
                                                          -- Unterbaeume + 1

{- Problem: (Knoten x [Leer, ..., Leer]) und (Knoten x []) sind zwei verschiedene Moeglichkeiten dafuer, ein Blatt zu sein (Unterschied: Knotengrad bzw. +/- externe Knoten i.S.v. Alp 3 (Schweppe); vgl. auch die Bsp. fuer BinBaeume in der Vorlesung mit denen fuer polymorphe Baeume). 
Wir versuchen, alle Funktionen fuer _beide_ Strukturen zu definieren.
preorder und hoehe sind bereits fuer _beide_ Strukturen korrekt definiert.
-}


-- a) 
wurzel :: (Baum a) -> a
wurzel Leer = error"Leerer Baum hat keine Wurzel"
wurzel (Knoten x xs) = x

-- ---------------------
-- b)
finde :: (Eq a) => a -> (Baum a) -> Bool
  -- stellt fest, ob sich ein Knoten mit der Beschriftung x im Baum t befindet
  -- das ist genau dann der Fall, wenn x in der Preorderliste von t ist
finde x t = x `elem` preorder t   

-- ---------------------
-- c) vater x t soll Beschriftung des Vaters eines (beliebigen) mit x 
--    beschrifteten Knotens ausgeben; Problem: Mehrfachvorkommen, unsere 
--    Loesung: wir nehmen den ersten mit x beschrifteten Knoten bei einer 
--    preorder-Traversierung, der von der Wurzel verschieden ist (die Wurzel hat 
--    keinen Vater und ist daher uninteressant). t muss mind. die Hoehe 1 haben. 
vater :: (Eq a) => a -> (Baum a) -> a
vater x t
  | hoehe t < 1 = error"Baum muss mind. die Hoehe 1 haben" 
  | not (or (map (finde x) (subTrees t))) = 
      error"Beschriftung kommt in den Unterbaeumen nicht vor" 
  | x `elem` kinder (wurzel t) t = wurzel t
          -- wg t >= 1 ist kinder (wurzel t) t /= [] 
  | otherwise = head (map (vater x) [s | s <- subTrees t, finde x s])
         -- x kommt in wenigstens einem Unterbaum von t vor, aber nicht in 
         -- deren Wurzeln. Wir untersuchen in preorder-Reihenfolge diejenigen 
         -- Unterbaeume, die x enthalten: map (vater x) liefert eine Liste der 
         -- Vaeter der x-Vorkommen, deren erstes gerade das gesuchte ist.
  
       
-- ----------------------
-- d) kinder x t gibt eine Liste der Beschriftungen der Kinder des in preorder-
--    Reihenfolge ersten mit x beschrifteten Knoten aus.
kinder :: (Eq a) => a -> (Baum a) -> [a]
kinder x t
   | isEmpty t = error"Leere Baeume haben keine Knoten mit Kindern"
   | not (finde x t) = error"Beschriftung kommt im Baum nicht vor"  
   | hoehe t == 0 = []   -- d.h. t = (Knoten x []) o. (Knoten x [Leer,...,Leer])
kinder x (Knoten y bs)   -- bs ist nicht-leer: /= [] u. /= [Leer, ..., Leer]
   | x == y = [wurzel t | t <- [s | s <- bs, not (isEmpty s)]] 
      -- die Wurzeln der nicht-leeren Elemente von bs bilden die gesuchte Liste  
   | otherwise = head ( map (kinder x) [s | s <- bs, finde x s])
      -- x /= y: wir durchlaufen diejenigen Elemente von bs, die x enthalten
      -- (rekursiv in preorder-Reihenfolge): [s | s <- bs, finde x s]. Da x in t 
      -- vorkommt und zugleich x /=y ist, muss x in wenigstens einem Teilbaum s
      -- aus bs vorkommen. 
      -- Das Erg. ist zunaechst eine Liste von Kindlisten zu den Knoten mit x. 
      -- Das erste Element dieser Liste ist die Kindliste des ersten Vorkommens
      -- von x (in Preorder-Reihenfolge). 
      

-- -------------------------
-- e) tiefe x t gibt die Tiefe eines mit x beschrifteten Knotens aus
--    Wir nehmen wieder den ersten Knoten dieser Art in preorder-Reihenfolge
tiefe :: (Eq a) => a -> (Baum a) -> Int
tiefe _ Leer = error "Leerer Baum: keine Knoten, deren Tiefe bestimmbar waere"
tiefe x t
  | not (finde x t) = error"Beschriftung kommt im Baum nicht vor"  
  | x == wurzel t = 0
  | otherwise = 1 + tiefe x (head [s | s <- subTrees t, finde x s])
     -- ist x nicht die Beschriftung der Wurzel von t, dann ist die Tiefe des 
     -- ersten x-Vorkommens gerade 1 + die Tiefe dieses Vorkommens in dem 
     -- ersten Unterbaum von t, der x enthaelt
  

-- -------------------------
-- f) cousins i x t gibt eine Liste der Cousins i-ten Grades eines mit x 
--    beschrifteten Knotens aus; Voraussetzung: keine Mehrfachvorkommen 
--    (sonst muss x selbst in der Liste vorkommen, die verschiedenen 
--    gleichnamigen Cousins waeren aber nicht mehr unterscheidbar)

cousins :: (Eq a) => Int -> a -> (Baum a) -> [a]      -- verlangt (Eq (Baum a))
cousins i x t
  |not(hatVorfahr (i+1) x t) = error"ohne (i+1)-ten Vorfahr keine i-ten Cousins"
  |otherwise = [y | y <- preorder t, y /= x, hatVorfahr (i+1) y t, 
                    vorfahr (i+1) y t == z, 
                    [] == [s | s <- subTreeX z t, wurzel s == vorfahr i x t, 
                           wurzel s == vorfahr i y t] 
               ]            
                   where 
                     z = vorfahr (i+1) x t
    -- d.h. kleinster gemeinsamer Vorfahr ist der (i+1)-te Vorfahr von x (=: z), 
    -- d.h. keine Wurzel der Unterbaeume des (i+1)-ten Vorfahren von x ist ein 
    -- gemeinsamer Vorfahr.

-- Hilfsfunktionen
      
isEmpty :: (Baum a) -> Bool
isEmpty Leer = True
isEmpty _ = False

{-
isLeaf :: (Eq a) => a -> (Baum a) -> Bool
isLeaf _ (Knoten _ ys) = ys == [] || [] == [s | s <- ys, s /= Leer]  
      -- setzt Eq (Baum a) voraus
isLeaf _ _ = False
-}

subTrees :: (Baum a) -> [Baum a]     -- liefert die Unterbaum-Liste eines Baums
subTrees Leer = error "Leere Baeume haben keine Subtree-Liste"
subTrees (Knoten _ bs) = bs

subTreeX :: (Eq a) => a -> (Baum a) -> [Baum a] -- Unterbaum-Liste eines Knotens
                                      -- pre: Knoten eindeutig im Baum
subTreeX _ Leer = error "Leere Baeume haben keine SubtreeX-Liste"
subTreeX x t
   | not (finde x t) = error "Beschriftung nicht im Baum"
   | x == wurzel t = subTrees t 
   | otherwise = head (map (subTreeX x) [s|s <- subTrees t, finde x s])
       -- ist x nicht die Wurzel, so muessen diejenigen Unterbaeume (rekursiv) 
       -- durchsucht werden, die x enthalten. Auf diese (nach Voraussetzung ex. 
       -- genau ein Unterbaum dieser Art) wird subTreeX angewandt, der Kopf der 
       -- resultierenden Liste von Unterbaumlisten ist die Unterbaumliste zu x. 
       -- Kommt x mehrfach vor, so wird die Liste zu dem in preorder-Rf. ersten 
       -- x-Vorkommen gefunden.

-- j-ter Vorfahr von x (dem in preorder-Rf.) ersten x-Vorkommen) in t;
vorfahr :: (Eq a) => Int -> a -> (Baum a) -> a
vorfahr _ _ Leer = error"Leerer Baum: hat keine untersuchbaren Knoten"
vorfahr j x t
  | not (hatVorfahr j x t) = error"Vorfahr existiert nicht"  -- i.ff.: j >= 1
  | j == 1 = vater x t       
  | otherwise = vorfahr (j-1) (vater x t) t
  
hatVorfahr :: (Eq a) => Int -> a -> (Baum a) -> Bool  
hatVorfahr j x t
  | (finde x t) && 0 < j && j <= tiefe x t = True
  | otherwise = False



-- ------------------------------------
-- Beispiele (zum Testen; tw. aus den Vorlesungsmaterialien):
-- ------------
tL :: Baum Int
tL = Leer

t0 = Knoten 1 []

t1 = Knoten 5 [Knoten 2 [],
              
              Knoten 7 
                      [Knoten 6 [],
                       Knoten 8 []
                      ]
             ]
 
--
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

{- ---- Tests -----------
wurzel: leerer Baum
	nichtleere Baeume unterschiedlicher Hoehe
	
finde: leerer Baum, Beschriftung nicht im (nicht-leeren) Baum
       Beschriftung der Wurzel, eines inneren Knotens, eines Blatts

vater: leerer Baum, Baum der Hoehe 0
       Beschriftung nicht in den Unterbaeumen
       Beschriftung auf Ebene 1, Beschriftung im Innern, B. eines Blatts

kinder: leerer Baum
        Beschriftung nicht im Baum
        Beschriftung der Wurzel, eines inneren Knotens, Suche nach Blatt

tiefe: leerer Baum
       Wurzel, innerer Knoten, Blatt
       
cousins: leerer Baum, Baum ohne 2. Generation, 
         Baum mit 2. Generation: x kommt vor / kommt nicht vor ...       	


-- ------- Testdoku --------
nicht getestet: mehrfach vorkommende Beschriftungen

-- wurzel:
Main> wurzel tL

Program error: Leerer Baum hat keine Wurzel

Main> wurzel t0
1
Main> wurzel t1
5

-- finde:
Main> finde 1 tL
False
Main> finde 1 t0
True
Main> finde 2 t0
False
Main> finde 7 t1
True
Main> finde 2 t1
True   



-- vater: 

Main> vater 1 tL

Program error: Baum muss mind. die Hoehe 1 haben

Main> vater 1 t0

Program error: Baum muss mind. die Hoehe 1 haben

Main> vater 1 t1

Program error: Beschriftung kommt in den Unterbaeumen nicht vor

Main> vater 5 t1

Program error: Beschriftung kommt in den Unterbaeumen nicht vor

Main> vater 2 t1
5
Main> vater 7 t1
5
Main> vater 8 t1
7    

-- kinder: 
Main> kinder 1 tL

Program error: Leere Baeume haben keine Knoten mit Kindern
  
Main> kinder 1 t1

Program error: Beschriftung kommt im Baum nicht vor

Main> kinder 8 t1
[]
Main> kinder 1 t0
[]
Main> kinder 5 t1
[2,7]
Main> kinder 7 t1
[6,8]

Main> tiefe 1 tL

Program error: Leerer Baum: keine Knoten, deren Tiefe bestimmbar waere

Main> tiefe 1 t0
0
Main> tiefe 1 t1

Program error: Beschriftung kommt im Baum nicht vor

Main> tiefe 7 t1
1
Main> tiefe 8 t1
2   

-- cousins (Fehlermeldungen sind noch nicht sonderlich instruktiv):

Main> cousins 0 1 tL

Program error: ohne (i+1)-ten Vorfahr keine i-ten Cousins

Main> cousins 1 1 tL

Program error: ohne (i+1)-ten Vorfahr keine i-ten Cousins

Main> cousins 0 1 t1

Program error: ohne (i+1)-ten Vorfahr keine i-ten Cousins

Main> cousins 0 2 t1

Program error: Vorfahr existiert nicht

Main> cousins 1 1 t1

Program error: ohne (i+1)-ten Vorfahr keine i-ten Cousins

Main> cousins 1 2 t1

Program error: ohne (i+1)-ten Vorfahr keine i-ten Cousins

Main> cousins 1 6 t1
[]
Main> cousins 1 "William" t2
["Beatrice","Eugenie","Peter","Zara"]
Main> cousins 1 "Eugenie" t2
["William","Henry","Peter","Zara"]

   
-}