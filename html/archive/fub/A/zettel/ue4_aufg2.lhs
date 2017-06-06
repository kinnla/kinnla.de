
======================================================================== 

Algorithmen und Programmieren I

Uebungszettel 4

Benjamin Jankovic, Christian Müller
________________________________________________________________________ 

Aufgabe 2:

Berechnung des ggT nach zwei verschiedenen Rekursionsalgorithmen

1. ggta
Die Aussage a = sb + r fuer r,s aus N mit r<b
entspricht      a = (div a b)*b + mod a b,
also ist ggt(b,r) = ggt(b, mod a b)

 >ggta::(Int,Int)->Int
 >ggta (a,b)
 >      | b==0          = a
 >      | otherwise 	= ggta (b, mod a b)

2.ggtb

 >ggtb::(Int,Int)->Int
 >ggtb (a,b)
 >      | b==0          = a
 >      | otherwise     = ggtb (b, abs (a-b))

Main> ggtb (10,15)
5
Main> ggta (10,15)
5
Main> ggtb (10,15)
5
Main> ggta (10,16)
2
Main> ggtb (10,16)
2
Main> ggta (1000000,10)
10
Main> ggtb (1000000,10)
10

Es faellt auf, dass ggta fuer sehr grosse Zahlenunterschiede deutlich  
schneller ist als ggtb.
Dies liegt daran, dass ggtb implizit auch mod a b berechnet, allerdings  
rekursiv mit Hilfe der Subtraktion. In Hugs ist 'mod' aber sicherlich  
nicht durch einen solchen rekursiven Algorithmus implementiert, sondern  
wird in konstanter Zeit berechenbar sein, wodurch ggta deutlich  
schneller ist.
