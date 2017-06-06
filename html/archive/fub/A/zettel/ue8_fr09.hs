
-- Aufgabe 3
-- =========

quicksort :: (a->a->Bool)-> [a] -> [a]
quicksort gk []     = []
quicksort gk (x:xs) = quicksort gk [y | y <- xs, y `gk` x]++[x]++ quicksort gk [y | y <- xs, not (gk y x)]


(##) :: String -> String -> Bool
[] ## _          = True
_ ## []          = False
(x:xs) ## (y:ys) = x<y || (x==y && (xs ## ys))


-- Aufgabe 4
-- =========

abl :: (Double->Double)->Double->Double->Double
abl f eps x
	| (eps > 0) = (f(x+eps)-f(x))/eps
	| otherwise = error "eps > 0"

-- Aufgabe 5
-- =========

texttoliste :: String -> [String]
texttoliste xs = words1 xs


words1    :: String -> [String]
words1 s    = case dropWhile isSpace1 s of
		  "" -> []
		  s' -> w : words1 s''
			where (w,s'') = break isSpace1 s'

isSpace1 c              = c == ' '      || c == '\t'     || c == '\n'     ||
			  c == '\r'     || c == '\f'     || c == '\v'     ||
			  c == '\xa0'   || c == '.'	 || c == ';'	  ||
			  c == '!'	|| c == '?'	 || c == '"'	  ||
			  c == ','

lexsort :: [String]->[String]
lexsort [] = []
lexsort (x:xs) = quicksort (##) (x:xs)

zaehlen :: [String]->[(Int,String)]
zaehlen [] = []
zaehlen (x:xs) = [(length[y | y <-x:xs, x == y],x)]++ zaehlen (drop ((length[y | y <-x:xs, x == y])-1) xs)

klein:: String->String
klein xs = map toLower xs

worthaeufig :: String -> [(Int,String)]
worthaeufig = quicksort (>=).zaehlen.lexsort.texttoliste.klein

text1="\nFest gemauert in der Erden\nSteht die Form, aus Lehm gebrannt.\nHeute muss die Glocke werden.\nFrisch Gesellen, seid zur Hand.\nVon der Stirne heiss\nRinnen muss der Schweiss,\nSoll das Werk den Meister loben,\nDoch der Segen kommt von oben.\n"

text2="\n\nWe skipped the light fandango\nturned cartwheels 'cross the floor\nI was feeling kinda seasick\nbut the crowd called out for more\nThe room was humming harder\nas the ceiling flew away\nWhen we called out for another drink\nthe waiter brought a tray\n\nAnd so it was that later\nas the miller told his tale\nthat her face, at first just ghostly,\nturned a whiter shade of pale\n\n "
