{- ======================================================
   Aufgabe 3)
   ======================================================-}
-- Standart QuickSorf Algorithmus, der als ersten Parameter eine
-- Funktion gk benötigt, die zum Vergleichen der Elemente verwendet
-- wird.
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort gk [] = []
quickSort gk (x:xs) = quickSort gk [y | y <- xs, not(gk x y)] ++ [x] ++ quickSort gk [y | y <- xs, gk x y]

{-
		Main> quickSort (>) [1,1,2,1,3,4]
		[4,3,2,1,1,1]
		Main> quickSort (<) [1,1,2,1,3,4]
		[1,1,1,2,3,4]
		Main> quickSort (==) [1,1,2,1,3,4]
		[4,3,2,1,1,1]
		Main> quickSort (<) ["david", "benjamin", "till"]
		["benjamin","david","till"]
		Main> quickSort (<) ["david", "benjamin", "till", "tobias"]
		["benjamin","david","till","tobias"]
		Main> quickSort (<) ["david", "benjamin", "tobias", "till"]
		["benjamin","david","till","tobias"]
		Main> quickSort (<) [4.2, 3.14, 2.82, 1.41]
		[1.41,2.82,3.14,4.2]
		Main> quickSort (>) [4.2, 3.14, 2.82, 1.41]
		[4.2,3.14,2.82,1.41]
		
	-}
{- ======================================================
   Aufgabe 4)
   ======================================================-}
-- Berechnet die Ableitung nach der gegebenen Formel...
abl :: (Double -> Double) -> Double -> Double -> Double
abl f eps x = ( ((f (x+eps)) - (f x)) / eps )

{- Beispiele:
	Main> abl sin 0.0001 0
	1.0
	Main> abl sin 0.0001 3.1415
	-0.998974
	Main> abl sin 0.00001 3.1415
	-0.953674
	Main> abl sin 0.00001 3.1415
	-1.00136
	Main> abl sin 0.00001 1
	0.542402
	Main> abl sin 0.00001 1.5
	0.0715256  -}

{- ======================================================
   Aufgabe 5)
   ======================================================-}
-- sortiert die Wörter eines Strings nach Häufigkeit unter dessen Angabe

-- Convert whole String to lower case
toLowerCase :: String -> String
toLowerCase = map toLower

-- split String into list of Strings (words)
makeWordList :: String -> [String]
makeWordList = words

-- drop all non-alphanumeric Chars
filterAlphaNum :: [String] -> [String]
filterAlphaNum = map (filter isAlphaNum)

-- drop all empty Strings (mostly produced by filterAlphaNum)
dropEmptyStrings :: [String] -> [String]
dropEmptyStrings inputList = [outputString | outputString <- inputList, length outputString > 0]

-- create list of "1"s with length of argument list and zip both to tuples
makeTuple :: [String] -> [(Int, String)]
makeTuple wordList = zip (replicate (length wordList) 1) wordList

-- accumulate the duplicates
countWords :: [(Int, String)] -> [(Int, String)]
countWords [] = []
countWords [a] = [a]
countWords ((i1, s1):(i2, s2):r)
	| s1 == s2 = countWords (((i1 + i2), s2):r)
-- same String count +1 and drop tuple, eval rest
	| otherwise = (i1, s1): countWords ((i2,s2):r)
-- keep tuple, eval rest

los :: [(Int, String)]
los = (quickSort (<) . countWords . makeTuple . quickSort (<) . dropEmptyStrings . filterAlphaNum . makeWordList . toLowerCase) testString

go :: String -> [(Int, String)]
go =	quickSort (<) . 					-- [(1,"a"),(1,"new"),(1,"old"),(1,"patch"),(1,"replaces"),(1,"with"),(2,"bugs")] 

			countWords . 							-- [(1,"a"),(2,"bugs"),(1,"new"),(1,"old"),(1,"patch"),(1,"replaces"),(1,"with")]
			
			makeTuple . 							-- [(1,"a"),(1,"bugs"),(1,"bugs"),(1,"new"),(1,"old"),(1,"patch"),(1,"replaces"),(1,"with")]
			
			quickSort (<) . 					-- ["a","bugs","bugs","new","old","patch","replaces","with"]
			
			dropEmptyStrings . 				-- ["a","patch","replaces","old","bugs","with","new","bugs"]
			
			filterAlphaNum . 					-- ["a","patch","replaces","old","bugs","with","new","bugs"]
			
			makeWordList . 						-- ["a","patch","replaces","old","bugs","with","new","bugs."]

			toLowerCase								-- "a patch replaces old bugs with new bugs."

																-- "A patch replaces old bugs with new bugs."
																

testString :: String
testString = "If you really want to hear about it, the first thing you'll probably want to know is where I was born, an what my lousy childhood was like, and how my parents were occupied and all before they had me, and all that David Copperfield kind of crap, but I don't feel like going into it, if you want to know the truth. In the first place, that stuff bores me, and in the second place, my parents would have about two hemorrhages apiece if I told anything pretty personal about them. They're quite touchy about anything like that, especially my father. They're nice and all--I'm not saying that--but they're also touchy as hell. Besides, I'm not going to tell you my whole goddam autobiography or anything. I'll just tell you about this madman stuff that happened to me around last Christmas just before I got pretty run-down and had to come out here and take it easy. I mean that's all I told D.B. about, and he's my brother and all. He's in Hollywood. That isn't too far from this crumby place, and he comes over and visits me practically every week end. He's going to drive me home when I go home next month maybe. He just got a Jaguar. One of those little English jobs that can do around two hundred miles an hour. It cost him damn near four thousand bucks. He's got a lot of dough, now. He didn't use to. He used to be just a regular writer, when he was home. He wrote this terrific book of short stories, The Secret Goldfish, in case you never heard of him. The best one in it was The Secret Goldfish. It was about this little kid that wouldn't let anybody look at his goldfish because he'd bought it with his own money. It killed me. Now he's out in Hollywood, D.B., being a prostitute. If there's one thing I hate, it's the movies. Don't even mention them to me.  Where I want to start telling is the day I left Pencey Prep. Pencey Prep is this school that's in Agerstown, Pennsylvania. You probably heard of it. You've probably seen the ads, anyway. They advertise in about a thousand magazines, always showing some hotshot guy on a horse jumping over a fence. Like as if all you ever did at Pencey was play polo all the time. I never even once saw a horse anywhere near the place. And underneath the guy on the horse's picture, it always says: Since 1888 we have been molding boys into splendid, clear-thinking young men. Strictly for the birds. They don't do any damn more molding at Pencey than they do at any other school. And I didn't know anybody there that was splendid and clear-thinking and all. Maybe two guys. If that many. And they probably came to Pencey that way.  Anyway, it was the Saturday of the football game with Saxon Hall. The game with Saxon Hall was supposed to be a very big deal around Pencey. It was the last game of the year, and you were supposed to commit suicide or something if old Pencey didn't win. I remember around three o'clock that afternoon I was standing way the hell up on top of Thomsen Hill, right next to this crazy cannon that was in the Revolutionary War and all. You could see the whole field from there, and you could see the two teams bashing each other all over the place. You couldn't see the grandstand too hot, but you could hear them all yelling, deep and terrific on the Pencey side, because practically the whole school except me was there, and scrawny and faggy on the Saxon Hall side, because the visiting team hardly ever brought many people with them.  There were never many girls at all at the football games. Only seniors were allowed to bring girls with them. It was a terrible school, no matter how you looked at it. I like to be somewhere at least where you can see a few girls around once in a while, even if they're only scratching their arms or blowing their noses or even just giggling or something. Old Selma Thurmer--she was the headmaster's daughter--showed up at the games quite often, but she wasn't exactly the type that drove you mad with desire. She was a pretty nice girl, though."
