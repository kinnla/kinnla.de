==========================================================================
ALP1 - ÜBUNG 6                                          Tutor: Till Zoppke
Nadine Alexander, Philipp Schmidt                                Gruppe 11
==========================================================================

Use radom list as test data

> import RandIntLists


Task 1
--------------------------------------------------------------------------
hanoi checks the input and calls _hanoi
--------------------------------------------------------------------------

> hanoi :: (Int, Int, Int) -> [(Int, Int)]
> hanoi (n, i, j) 

we forbid less than one circle

>              | (n < 1) = error "put yoursef nowhere where you came from"

nor do we have other stacks than 1,2,3

>              | (i > 3) || (j > 3) = error "i knew where to put the circles if i knew where to put you"
>              | (i < 1) || (j < 1) = error "i knew where to put the circles if i knew where to put you" 

moveing the stack is doing nothing

>              | (i == j) = error "nothing to do"

we managed to have right input data

>              | otherwise = _hanoi (n, i, j)



now to the real function....
--------------------------------------------------------------------------
_hanoi moves n circles from stack i to j
--------------------------------------------------------------------------
Using recursion this is simple, move n-1 circles away on the third stack,
move the bottomost, then move n-1 on top....
This algorithem never violates the rules of the game. 

> _hanoi :: (Int, Int, Int) -> [(Int, Int)]

move a single circle simply without care about nothing

> _hanoi (1, i, j) = [(i, j)]

move n-1 circles to the other stack, then move the singe circle to 
its given destiantion, at last move the n-1 circles back on to

> _hanoi (n, i, j) = _hanoi (n-1, i, k)++[(i, j)]++_hanoi (n-1, k, j)

get the other stack - nice with list comprehension

>        where [k] = [ o | o <- [1,2,3], o /= i, o/= j]


testrun:

Main> hanoi (5,0,7)

Program error: i knew where to put the circles if i knew where to put you
(40 reductions, 131 cells)

Main> hanoi (3,1,2)
[(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2)]
(387 reductions, 671 cells)
Main> hanoi (5,1,2)
[(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2),(1,3),(2,3),(2,1),(3,1),(2,3),
(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2),(3,1),(2,3),(2,1),(3,1),(3,2),
(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2)]
(1607 reductions, 2891 cells)
Main> hanoi (-3,1,2)

Program error: put yoursef nowhere where you came from
(20 reductions, 92 cells)





Task 2
--------------------------------------------------------------------------
match u v returns True if u is at least one time(s) substring of v
--------------------------------------------------------------------------

> match :: String -> String -> Bool

nothing ever fits in between

> match [] _ = True

nothing fits in between nothing

> match _ [] = False

if the first character matches and all flollowing, or the next (or the 
next of the next) and all flollowing them, the tring is substring.  

> match (u:us) (v:vs) = (u == v && match us vs) || (match (u:us) vs)


testruns:

Main> match "uff" "Badsalzufflen"
True
(47 reductions, 107 cells)
Main> match "uff" "Kartoffeln"
False
(47 reductions, 107 cells)





--------------------------------------------------------------------------
shmatch u v sets every occurance of the substring u in v in " *<u>* "
--------------------------------------------------------------------------


> shmatch :: String -> String -> String

the empty string is in every string - return the orignal string 

> shmatch [] s = s

empty stays empty

> shmatch _ [] = []

now we care about "normal" strings

> shmatch (u:us) (v:vs)

       u:us is substring of v:vs ans begins at v, put it in signs
       defined below and care about the rest of the string after 
       that substring by calling shmatch recursively on it

>      | msh = lmk++(u:us)++rmk++(shmatch (u:us) (drop (length (u:us)) (v:vs)))

       u:us is not a substring or not starting at v - try for vs

>      | otherwise = v:(shmatch (u:us) vs)

>      where msh = ms (u:us) (v:vs) -- tell me string matches here

             catch if/that it matched

>            ms :: String -> String -> Bool

             empty string matches always, beacuse we want only the 
             matches string, cut off the rest

>            ms [] _      = True 

             nothing can be matched 

>            ms _  []     = False 

             we have normal strings....

>            ms (u:us) (v:vs)

                first character matches - recursively check the rest

>               | u == v = ms us vs

                one charakter did not match - tell it to the caller

>               | otherwise = False

             some constants - i used " *" instead of "*" because its easier
             to recognize - change them or leave it

>            lmk = " *"
>            rmk = "* "


testruns:

Main> shmatch "nacht" "weihnachten"
"weih *nacht* en"
(485 reductions, 774 cells)
Main> shmatch "tz" "adtzwentzkrantzkertze"
"ad *tz* wen *tz* kran *tz* ker *tz* e"
(1042 reductions, 1653 cells)
Main> shmatch "tz" "tztztztztztztztz"
" *tz*  *tz*  *tz*  *tz*  *tz*  *tz*  *tz*  *tz* "
(1496 reductions, 2187 cells)
Main> shmatch "e" "elefanten"
" *e* l *e* fant *e* n"
(559 reductions, 899 cells)





Task 3
now to ordering and sorting - we don't only sort Int's - we can sort 
everything which has a less-or-equal-than defined, this is every Type
which fits into the builtin Class Ord. So we use the porymorph data 
type 'a' which is an instance of Ord
--------------------------------------------------------------------------
mini removes the smallest element from a list and returns a tupel of the 
     smallest element and the rest of the list.
--------------------------------------------------------------------------

> mini :: Ord a => [a] -> (a, [a])

there is no minimum of an empty list:

> mini [] = error "which minimum?"

if the List contains only one element it must be the minimum

> mini [e] = (e, [])

otherwise check if the element is smaller than the smallest of the 
rest of the list

> mini (e:es) 
>          | e <= fstmin = (e, es)
>          | otherwise     = (fstmin, (e:sndmin))

performance issue: call mini recursive only once 

>          where (fstmin, sndmin) = mini es


testruns:

Main> minis [98,23,26,5,2,964]
[2,5,23,26,98,964]
(268 reductions, 476 cells)
Main> mini rand10
(1,[5,2,6,4,5,2,9,9,2])
(197 reductions, 355 cells)
Main> mini rand100
(1,[64,94,80,94,44,77,16,57,75,28,79,94,43,3,12,81,9,87,98,38,38,38,57,55,4,6,2,
25,47,15,33,89,59,56,11,87,33,89,83,33,63,37,10,54,21,32,41,47,20,90,61,81,63,93
,98,73,33,84,30,10,53,22,47,28,57,100,88,15,96,82,89,93,11,77,75,41,26,81,93,9,3
3,100,6,84,67,80,70,61,51,50,3,99,65,51,20,65,7,94,43])
(1916 reductions, 3505 cells)
Main> mini [600,599..1]
(1,[600,599,598,597,596,595,594,593,592,591,590,589,588,587,586,585,584,583,582,
581,580,579,578,577,576,575,574,573,572,571,570,569,568,567,566,565,564,563,562,
561,560,559,558,557,556,555,554,553,552,551,550,549,548,547,546,545,544,543,542,
541,540,539,538,537,536,535,534,533,532,531,530,529,528,527,526,525,524,523,522,
521,520,519,518,517,516,515,514,513,512,511,510,509,508,507,506,505,504,503,502,
501,500,499,498,497,496,495,494,493,492,491,490,489,488,487,486,485,484,483,482,
481,480,479,478,477,476,475,474,473,472,471,470,469,468,467,466,465,464,463,462,
461,460,459,458,457,456,455,454,453,452,451,450,449,448,447,446,445,444,443,442,
441,440,439,438,437,436,435,434,433,432,431,430,429,428,427,426,425,424,423,422,
421,420,419,418,417,416,415,414,413,412,411,410,409,408,407,406,405,404,403,402,
401,400,399,398,397,396,395,394,393,392,391,390,389,388,387,386,385,384,383,382,
381,380,379,378,377,376,375,374,373,372,371,370,369,368,367,366,365,364,363,362,
361,360,359,358,357,356,355,354,353,352,351,350,349,348,347,346,345,344,343,342,
341,340,339,338,337,336,335,334,333,332,331,330,329,328,327,326,325,324,323,322,
321,320,319,318,317,316,315,314,313,312,311,310,309,308,307,306,305,304,303,302,
301,300,299,298,297,296,295,294,293,292,291,290,289,288,287,286,285,284,283,282,
281,280,279,278,277,276,275,274,273,272,271,270,269,268,267,266,265,264,263,262,
261,260,259,258,257,256,255,254,253,252,251,250,249,248,247,246,245,244,243,242,
241,240,239,238,237,236,235,234,233,232,231,230,229,228,227,226,225,224,223,222,
221,220,219,218,217,216,215,214,213,212,211,210,209,208,207,206,205,204,203,202,
201,200,199,198,197,196,195,194,193,192,191,190,189,188,187,186,185,184,183,182,
181,180,179,178,177,176,175,174,173,172,171,170,169,168,167,166,165,164,163,162,
161,160,159,158,157,156,155,154,153,152,151,150,149,148,147,146,145,144,143,142,
141,140,139,138,137,136,135,134,133,132,131,130,129,128,127,126,125,124,123,122,
121,120,119,118,117,116,115,114,113,112,111,110,109,108,107,106,105,104,103,102,
101,100,99,98,97,96,95,94,93,92,91,90,89,88,87,86,85,84,83,82,81,80,79,78,77,76,
75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49
,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,2
2,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2])
(19244 reductions, 31770 cells)
Main>






--------------------------------------------------------------------------
minis sorts a list of Int by usung mini
--------------------------------------------------------------------------

> minis :: Ord a => [a] -> [a]

if the list is empty, return the empty list 

> minis [] = []

if the list contains only on element, it must be alredy sorted somehow

> minis [e]  = [e]

put the smallest on top of the list and sort the rest

> minis es = fstmin : minis sndmin

performance issue: call mini recursive only once 

>          where (fstmin, sndmin) = mini es


testrun:

Main> minis rand100
[1,2,3,3,4,6,6,7,9,9,10,10,11,11,12,15,15,16,20,20,21,22,25,26,28,28,30,
32,33,33,33,33,33,37,38,38,38,41,41,43,43,44,47,47,47,50,51,51,53,54,55,
56,57,57,57,59,61,61,63,63,64,65,65,67,70,73,75,75,77,77,79,80,80,81,81,
81,82,83,84,84,87,87,88,89,89,89,90,93,93,93,94,94,94,94,96,98,98,99,100,
100]
(57987 reductions, 105880 cells)
Main> minis rand10 
[1,2,2,2,4,5,5,6,9,9]
(661 reductions, 1203 cells)


The function needs polinominal more time for bigger lists, because 
it must go through the list n²-n times. If you wanted concret times,
minis need for 10 elements about tenth a secon, for 100 about one second.

For really long lists (depending on the compiler/interpreter version 
from about 900 to 7500 elements) you get a control stack overflow. 

    Elements    | Reverse Sort   | Sort          | Random
   -------------+----------------+---------------+---------------
            10  |            692 |           833 |          667
           100  |          60813 |         57480 |        58188

   -> Reductions

just 4 phun.....
--------------------------------------------------------------------------
dreichi - we play "3 chinese with contrabass"
--------------------------------------------------------------------------

> dreichi :: Char -> String -> String
> dreichi c [] = []
> dreichi c (s:se)
>                | elem s "aeiouäöüAEIOUÄÖÜ" = c:(dreichi c se)
>                | otherwise = s:(dreichi c se)

