------------- Programmiersession vom Donnerstag, dem 31.10.2002 ------------

-- ggT ===========================
ggT :: (Int, Int) -> Int
ggT(i,j)
	|j > i = ggT(j,i)             -- Vertausche beide Zahlen, falls die Erste 
	                              -- kleiner als die Zweite ist.
	|(mod i j == 0) = j           -- Abbruchbedingung, Ergebnis zurueckliefern
	|otherwise =  ggT(mod i j, j) -- rekursiver Aufruf


-- Rechnet die logische Implikation aus ======
implikation :: (Bool, Bool) -> Bool
implikation(a,b)
	|(a == b) = True       -- Fall: (a=1, b=1); (a=0, b=0)
	|b = True              -- Fall: (a=0, b=1) 
	|otherwise = False     -- Fall: (a=1, b=0)


-- Rechnet die logische Implikation aus ======
implikation1 :: (Bool, Bool) -> Bool
implikation1(a,b) = a == b || b     -- geschlossene Formel


-- Testlaeufe 
	{-

	Main> ggT (32768,384)
	128
	Main> ggT (15,10)
	5
	Main> ggT (10,15)
	5
	

	Main> implikation (True, False)
	False
	Main> implikation (True, True)
	True
	Main> implikation (False, True)
	True
	Main> implikation (False, False)
	True


	Main> implikation1 (False, False)
	True
	Main> implikation1 (True, False)
	False
	Main> implikation1 (True, True)
	True
	Main> implikation1 (False, True)
	True

	-}

