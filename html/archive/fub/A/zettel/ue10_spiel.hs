-- Typklasse Gruppe
-- definiert die Verknüpfung 'Kringel', sowie neutrales und inverses Element
class (Eq a) => Gruppe a where
	(...) :: a -> a -> a
	neutral :: a
	invers :: a -> a

-- Int als Instanz von Gruppe, als sogenannte Additive Gruppe.
instance Gruppe Int where
	(...) = (+)
	neutral = constNull
	invers = negate

-- Float als Instanz von Gruppe, als sogenannte Multiplikative Gruppe.
instance Gruppe Float where
	(...) = (*)
	neutral = 1
	invers = recip


-------------------------- Visible aus der Vorlesung ----------------------------

-- Typklasse Visible
class Visible	a where
	toString :: a -> String
	size :: a -> Int

-- Char als Instanz von Visible
instance Visible Char where
	toString ch = [ch]
	size _  = 1

-- Bool als Instanz von Visible
instance Visible Bool where
	toString True = "True"
	toString False = "False"
	size _ = 1

-- Int als Instanz von Visible
-- Das ist nicht die Loesung von 3a) !!!
instance Visible Int where
  toString = show
  size = id

-- Listen als Instanz von Visible
instance Visible a => Visible [a] where
	toString = concat . map toString
	size = foldr (+) 1 . map size

-- ein Zweitupel als Instanz von Visible
instance (Visible a, Visible b) => Visible (a,b)  where
	toString (a,b) = "(" ++ toString a ++ "," ++ toString b ++ ")"
	size (a,b) = size a + size b + 3

-- Auch Funktionen können Instanzen von Typklassen sein
-- Der Typ der Urbildmenge soll zusätzlich zu Visible 
-- auch Instanz von Bounded und Enum sein
instance (Visible a, Bounded a, Enum a, Visible b) => Visible (a -> b) where
  
  -- toString erzeugt eine Liste aller Funktionswerte
  toString f = toString $ map f [minBound .. maxBound]
  
  -- size summiert über size eines jeden Elementes
  size f = size $ map f [minBound .. maxBound]

----------------------------- zum Testen --------------------------------

-- Trick für die Ausgabe
constNull :: Int
constNull = 0

-- eine Visible Funktion. Das mit minBound sieht etwas komisch aus.
dummie :: Char -> String
dummie minBound = "m" 
dummie _ = "nicht minBound"

-- Eine Visible Funktion. 
dummie2 :: Int -> String
dummie2 (-2147483647) = "hier ist minBound\n"
dummie2 (-2146483647) = "schon eine Million geschafft\n"
dummie2 (-1147483647) = "schon eine Milliarde geschafft\n"
dummie2 0 = "noch immer nicht abgestuerzt?\n"
dummie2 _ = ""

-- identität für Int
intId :: Int -> Int
intId = id


----------------------------- Testläufe -----------------------------------
{-

Main> putStr $ toString dummie2
hier ist Minbound
schon eine Million geschafft
{Interrupted!}

(58182090 reductions, 78717218 cells, 329 garbage collections)
Main> putStr $ toString dummie
mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
mmmmmmmmmmmmm
(8753 reductions, 12647 cells)
Main> size dummie
513
(8760 reductions, 11114 cells)   
Main> toString toLower
"\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB
\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrst
uvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL\128\129\130\131\132\133\134\135\1
36\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\15
6\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176
\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\224\225\226\227\228\
229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\215\248\2
49\250\251\252\253\254\223\224\225\226\227\228\229\230\231\232\233\234\235\236\23
7\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255"
(32263 reductions, 41570 cells)      
Main> toString 0
ERROR - Unresolved overloading
*** Type       : (Num a, Visible a) => [Char]
*** Expression : toString 0

Main> toString constNull
"0"
(44 reductions, 61 cells)                 
Main> constNull ... 3
3
(16 reductions, 24 cells)          
Main> toString (123 + constNull, ord)
"(123,012345678910111213141516171819202122232425262728293031323334353637383940414
243444546474849505152535455565758596061626364656667686970717273747576777879808182
838485868788899091929394959697989910010110210310410510610710810911011111211311411
511611711811912012112212312412512612712812913013113213313413513613713813914014114
214314414514614714814915015115215315415515615715815916016116216316416516616716816
917017117217317417517617717817918018118218318418518618718818919019119219319419519
619719819920020120220320420520620720820921021121221321421521621721821922022122222
322422522622722822923023123223323423523623723823924024124224324424524624724824925
0251252253254255)"
(21026 reductions, 30008 cells)         
Main> putStr $ toString $ map ((++ "\n") . toString) [(||), (&&), min, max, (==), (>=), (<=), (<), (>), max.not, (\x y -> True)] 
FalseTrueTrueTrue
FalseFalseFalseTrue
FalseFalseFalseTrue
FalseTrueTrueTrue
TrueFalseFalseTrue
TrueFalseTrueTrue
TrueTrueFalseTrue
FalseTrueFalseFalse
FalseFalseTrueFalse
TrueTrueFalseTrue
TrueTrueTrueTrue

(3056 reductions, 6489 cells)
Main> 

"je True desto kurz"
-}