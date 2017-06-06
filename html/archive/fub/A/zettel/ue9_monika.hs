{- ==============================================================
Alp 1 - WS 02/03 (Tutorium: Till Zoppke)              Do_12
Monika Budde / Emrah Somay 
Uebung 9, Aufg. 3 


============================================================== -}
-- a) anzahl x xs berechnet die Anzahl der Vorkommen von x in der Liste xs. 
--    Diese ist 0, wenn xs leer ist. Andernfalls ist der Kopf der Liste xs 
--    mit x zu vergleichen: ist dieser gleich x, so erhoeht sich die Anzahl 
--    der Vorkommen von x in der Restliste um 1, sonst ist einfach die Anzahl
--    der Restliste auszugeben.
--    Um x mit den Elementen von xs vergleichen zu koennen, muessen x und die 
--    Elemente von xs zu einem Eq-Typ gehoeren.

anzahl :: Eq a => a -> [a] -> Int
anzahl _ [] = 0
anzahl x (y:ys) 
  | x == y = 1 + anzahl x ys  
  | otherwise = anzahl x ys
  
  
-- b) ueberein xs ys gibt eine Boolesche Liste aus, die angibt, wo die Listen  
--    xs und ys uebereinstimmen. Ist eine der Listen leer, so ist auch die 
--    Ergebnisliste leer. Andernfalls werden die Listen elementweise 
--    miteinander verglichen, bis die Restliste von einer oder beiden Listen 
--    leer ist.
--    Der Vergleich der Listenelemente setzt voraus, dass die Listenelemente
--    zur Klasse Eq gehoeren; damit "==" anwendbar ist, muessen die zu 
--    vergleichenden Listen ausserdem vom selben Typ sind. 

ueberein :: Eq a => [a] -> [a] -> [Bool]
ueberein [] _ = []
ueberein _ [] = []
ueberein (x:xs) (y:ys)
  | x == y = True : ueberein xs ys
  | otherwise = False : ueberein xs ys

{- **************
*** Tests

zu a)
Testdaten: 
Listen verschiedener Typen, jeweils [], einelementige Liste mit / ohne 
     gesuchtem Element, mehrelementige Liste mit / ohne gesuchtem Element

***Testdokumentation***
Z:\ALP_Uebgen\Alp1_Ue9\aufg9_3.hs
Main> anzahl 5 []
0
Main> anzahl 'a' []
0
Main> anzahl "adfa" []
0
Main> anzahl 5 [5]
1
Main> anzahl 5 [6]
0
Main> anzahl 5 [2,3,5,6]
1
Main> anzahl 5 [5,3,5,6]
2
Main> anzahl 5 [3,6]
0
Main> anzahl 'a' "asdlfkazer"
2
Main> anzahl 'a' "a"
1
Main> anzahl 'a' "sdlfkzer"
0
Main>
                     

-------------------
zu b)
Testdaten:
- je eine der Listen ist Leer
- gleich lange Listen: einelementige / mehrelementige, die vollstaendig / 
  teilweise / nirgends uebereinstimmen
- unterschiedlich lange Listen ...

***Testdokumentation***

Main> ueberein [5] []
[]
Main> ueberein [] [5]
[]
Main> ueberein [1,5,7] [5]
[False]
Main> ueberein [5,1,7] [5]
[True]
Main> ueberein [5,1,7] [5, 6,]
ERROR - Syntax error in expression (unexpected `]')
Main> ueberein [5,1,7] [5, 6]
[True,False]
Main> ueberein [5,1,7] [5,1,7]
[True,True,True]
Main>       

-}