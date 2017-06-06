{- ==============================================================
Alp 1 - WS 02/03 (Tutorium: Till Zoppke)              Do_12
Monika Budde / Emrah Somay 
Uebung 7, Aufg. 3.b) 

anfVor liefert fuer einen beliebigen String xs und ein Element x das Anfangsstueck von xs bis zum ersten Auftreten von x, falls x vorkommt, und [] sonst (falls fuer xs definiert). Wir benutzen die Hilfsfunktion contains. "bis" verstehen wir inklusiv, es wird also das Anfangsstueck einschliesslich x ausgegeben.

Wenn unendliche Strings xs betrachtet werden, dann ist anfVor x xs nur definiert, falls x in xs vorkommt: andernfalls ist bereits contains nicht definiert, d.h. (6) und damit auch (3) terminiert nicht.

Korrektheitsbeweis: s. Extrazettel zu Aufg. 1-3

============================================================== -}

anfVor :: Char -> String -> String
anfVor x [] = []                                -- (1) Sonderfall 
anfVor x (y:ys) 
    | x == y = [x]                              -- (2) Rekursionsanker
    | contains ys x = y:(anfVor x ys)           -- (3) x /= y
    | otherwise = []                            -- (4) x not in y:ys


-- Hilfsfunktion
-- contains xs x == True iff xs contains x

contains :: Eq a => [a] -> a -> Bool 
contains [] _ = False                           -- (5) Rekursionsanker
contains (y:ys) x = (y == x || contains ys x)   -- (6)

{- ***************
** Testfaelle **
-- endliche Listen
'a' []          -- Fall (1) bzw. (5)
'a' ['a'..'z']  -- Fall (2) bzw. (6) mit 1. Adjunktionsglied == True
'b' ['a'..'z']  -- Fall (3): Randwert (1 Rekursionsaufruf), 
                             bzw. (6) mit 2. Adjunktionsglied == True
'm' ['a'..'z']  -- Fall (3): mittlerer Wert
'z' ['a'..'z']  -- Fall (3): Randwert (am Listenende)
'Z' ['a'..'z']  -- Fall (4) bzw. (6) mit Disjunktion == False (hinten)
'a' ['b'..'z']  -- Fall (4) bzw. (6) mit Disjunktion == False (vorne)

-- unendliche Listen
'a' ['a', 'a' .. ]
'b' ['a', 'a' .. ]             -- nicht definiert
'b' (['a'..'z']++['a', 'a' .. ])
'a' (['b'..'z']++['a', 'a' .. ])

** Testdokumentation **
** Test fuer contains: endliche Listen **

Main> contains [] 'a'
False
Main> contains ['a'..'z'] 'a'
True
Main> contains ['a'..'z'] 'b'
True
Main> contains ['a'..'z'] 'm'
True
Main> contains ['a'..'z'] 'z'
True
Main> contains ['b'..'z'] 'a'
False
Main>   

** Test fuer contains: unendliche Listen **

Main> contains ['a', 'a' .. ] 'a'
True
Main> contains ['a', 'a' .. ] 'b'
{Interrupted!}

Main> contains (['a'..'z']++['a', 'a' .. ]) 'b'
True
Main> contains (['b'..'z']++['a', 'a' .. ]) 'a'
True
Main>  


** fuer anfVor: endliche Listen**

Main> anfVor 'a' []
""
Main> anfVor 'a' ['a'..'z']
"a"
Main> anfVor 'b' ['a'..'z']
"ab"
Main> anfVor 'm' ['a'..'z']
"abcdefghijklm"
Main> anfVor 'z' ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
Main> anfVor 'Z' ['a'..'z']
""
Main> anfVor 'a' ['b'..'z']
""
Main>  

** Test fuer anfVor: unendliche Listen **
Main> anfVor 'a' ['a', 'a' .. ]
"a"
Main> anfVor 'b' ['a', 'a' .. ]
"{Interrupted!}

Main> anfVor 'b' (['a'..'z']++['a', 'a' .. ])
"ab"
Main> anfVor 'a' (['b'..'z']++['a', 'a' .. ])
"bcdefghijklmnopqrstuvwxyza"
Main>  
 
-}