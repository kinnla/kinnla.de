-------- Aufgabe 1 ----------------------
hanoia :: (Int, Int, Int) -> [(Int, Int)]
hanoia (1, i, j) = [(i, j)]
hanoia (n, i, j) = hanoi (n-1, i, frei) ++ [(i, j)] ++ hanoi (n-1, frei, j)
  where frei = 6 - i - j

-- Berechnet die Züge, um einen Turm mit n Scheiben nach
-- dem Problem von Hanoi von Position i nach j zu bewegen
hanoi::(Int,Int,Int)->[(Int,Int)]
hanoi (n,i,j)
        -- mindestens eine Scheibe und zwei Positionen im
        -- Bereich von 1-3 müssen gegeben sein
        | n<0 || i<1 || i>3 || j<1 || j>3  = error "Argumente nicht im Definitionsbereich."
        |i==j                              = error "Nichts zu tun."
        -- eigentliche Berechnung starten
        |otherwise                         = hanoi_hilf(n,i,j,6-i-j)

        
-- Hilfsfunktion, die die Züge über die vorhandene
-- Hilfs-Position h berechnet
hanoi_hilf::(Int,Int,Int,Int)->[(Int,Int)]
-- Rekursionsverankerung
hanoi_hilf (0,_,_,_) = []
-- bewege n-1 Scheiben von i nach h über j
hanoi_hilf (n,i,j,h) = hanoi_hilf(n-1,i,h,j)++
                       [(i,j)]++
                       hanoi_hilf(n-1,h,j,i)


-------- Aufgabe 2a ---------------------

-- prüft, ob ein String als Teil-String in einem
-- anderen String enthalten ist
match::String->String->Bool
-- starte beim nullten Zeichen
match u v = fst (match_hilf u v 0)


-- Liefert ein Tupel, das aussagt, ob ein String
-- enthalten ist und an welcher Position
match_hilf::String->String->Int->(Bool,Int)
-- der leere String ist am Anfang enhalten
match_hilf [] _ _			= (True, 0)
-- eine leerer String enthält keinen anderen String
match_hilf _ [] _			= (False, 0)
match_hilf u (v:vs) p
        -- eine Position kann nur positiv sein
	|p<0				= error "p<0"
        -- wenn u kleier ist als v, dann k u nicht in v
        -- enthalten sein
	|(length u) > (length (v:vs))	= (False, 0)
        -- wenn u am Anfang des Strings enthalten ist,
        -- dann gebe True und die Position zurück
	|u==(take (length u) (v:vs))	= (True, p)
        -- prüfe vs und erhöhe die Position um 1
	|otherwise			= match_hilf u vs (p+1)


------- Aufgabe 2b -----------------------

-- markiert alle Vorkommen des Strings u im String v
match_mark::String->String->String
-- ein leerer String kann keinen anderen enthalten
match_mark _ [] = ""
-- der leere String ist immer am Anfang des Strings v
-- enthalten
match_mark [] v = "**"++v
match_mark u v
        -- wenn u länger als v dann kann u nicht in v enthalten sein
	|length u > length v	= v
	|wahr/=True		= v
	|otherwise		= take pos v ++ "*" ++ 
				  take (length u) (drop pos v) ++ "*" ++ 
				  match_mark u (drop (pos + length u) v)
		where
		tupel = match_hilf u v 0
		wahr  = fst tupel
		pos   = snd tupel


------ Aufgabe 3a ----------------------

-- liefert das Minimun der Liste und die Liste ohne
-- das entfernte Minimum selbst
mini::[Int]->(Int,[Int])
mini [] = error "Leere Liste"
mini x = (m, entf m x)
	where m = minimum x


-- entfernt einen Wert aus einer Liste
entf::Int->[Int]->[Int]
-- die leere Liste enthält keine Werte
entf _ []		= []
entf i (x:xs)
        -- wenn das erste Element das gesuchte ist,
        -- dann gebe die Restliste zurück
	|i==x		= xs
	|otherwise	= x : entf i xs


---- Aufgabe 3b --------------------

-- sortiert eine Liste aufsteigend
sort::[Int]->[Int]
-- die leere Liste ist bereits sortiert
sort [] = []
sort x = fst m : sort (snd m)
	where m = mini x

        
        
{-
---- TESTLÄUFE
Main> hanoi (3,1,2)
[(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2)]
Main> hanoi (3,3,2)
[(3,2),(3,1),(2,1),(3,2),(1,3),(1,2),(3,2)]
Main> hanoi (0,1,2)
[]
Main> hanoi (0,1,1)

Program error: Nichts zu tun.


Main> match "o W" "Hallo Welt"
True
Main> match "hal" "Hallo"
False
Main> match "" "Hallo"
True
Main> match "blubb" ""
False


Main> match_mark "33" "333333333"
"*33**33**33**33*3"
Main> match_mark "bla" "bla bla blubb b laber"
"*bla* *bla* blubb b laber"
Main> match_mark "*" "irgs"
"irgs"
Main> match_mark "*" "ir*gs"
"ir***gs"
Main> match_mark "" "ir*gs"
"**ir*gs"
Main> match_mark "Mafi Tutorium" ""
""


Main> mini [5,4,1,5,6,1]
(1,[5,4,5,6,1])
Main> mini (snd (mini [5,4,1,5,6,1]))
(1,[5,4,5,6])
Main> mini []

Program error: Leere Liste


Main> sort [5,6,4,2,5,7,8,5,3,5,6,2]
[2,2,3,4,5,5,5,5,6,6,7,8]
Main> sort []
[]
-}