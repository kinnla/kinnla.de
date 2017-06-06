-- Simples Programm zur Berechnung. Für große Zahlen nicht effizient.
rechne1, rechne2 :: (Integer -> Integer) -> Integer -> Integer -> Integer
rechne1 f maxi n = until (\ x -> f x > k || x > maxi) (+1) n - 1
  where k = f n * 1000

-- Kompliziertes Programm zur Berechnung, mit Fibonacci-Intervallschachtelung.
rechne2 f maxi n = rec n (fibo 1) 0
  where k = f n * 1000

        -- k      : Konstante für f(n)*1000
        -- x      : aktueller Wert aus der Eingabemenge
        -- (l:ls) : Liste der Zahlen, die zu x zu addieren sind
        -- i      : Anzahl der Iterationen
        rec :: Integer -> [Integer] -> Integer -> Integer
        rec x (l:ls) i
          | f_x == k                  = x
          | i == maxi                 = maxi
          | not turnAround            = rec (x+l) ls (i+1)
          | turnAround && singleStep  = x + etwas
          | otherwise                 = rec x newList (i+1) 

            where newList = fibo (-sig_l)
                  etwas = if l==1 then -1 else 0
                  f_x = f x
                  singleStep = abs l == 1
                  turnAround = signum (f_x - k) == sig_l
                  sig_l = signum l

-- unendliche Liste von Fibonacci-Zahlen
fibo :: Integer -> [Integer]
fibo n = fibohelp [n, n]
fibohelp [a, b] = a : fibohelp [b, a+b]

-- Funktionen vom Aufgabenblatt
-- leider gibt es keinen unbegrenzten Floatingpoint-Datentyp in Haskell
-- (also ein Pendant zu Integer). Deshalb sind die Berechnungen ungenau.
a_,b_,c_,d_,e_,f_,g_,h_ :: Integer -> Integer
a_ = (^3)
b_ = truncate . logBase 2 . fromInteger
c_ n = (9^n) `div` (5^n)
d_ = id
e_ = (3^)
f_ = truncate . sqrt . fromInteger
g_ n = truncate $ i * (logBase 2 i) ^ 2 where i = fromInteger n
h_ = (^2)

funktionen :: [Integer -> Integer]
funktionen = [a_,b_,c_,d_,e_,f_,g_,h_]

-- Eingaben vom Aufgabenblatt
eingaben :: [Integer]
eingaben = [20, 1000]

-- Maximale Anzahl von Iterationen
maxi1, maxi2 :: Integer
maxi1 = 300010
maxi2 = 500

-- Ergebnisse der Berechnungen
tabelle :: ((Integer -> Integer) -> Integer -> Integer -> Integer) -> Integer
           -> [Integer -> Integer] -> [Integer] -> String
tabelle rechne maxi flist nlist  = unlines $ map zeile flist
  where zeile f = unwords $ map ((++"\t") . showme . rechne f maxi) nlist
          where showme x = if x == maxi
                           then "---"
                           else show x

-- kurzer Aufruf zum Starten
start1, start2 :: IO()
start1 = putStr $ tabelle rechne1 maxi1 funktionen eingaben
start2 = putStr $ tabelle rechne2 maxi2 funktionen eingaben

{-
Hugs session for:
/usr/lib/hugs/lib/Prelude.hs
laufzeit.hs
Main> start1
200      10000
---      ---
31       1011
20000    ---
26       1006
---      ---
2835     300007
632      31622

(290072884 reductions, 443446305 cells, 1859 garbage collections)
Main> start2
200      10000
---      ---
31       1011
20000    1000000
26       1006
16005197         961003628
2835     300007
632      31622

(433577 reductions, 766169 cells, 3 garbage collections)
-}
