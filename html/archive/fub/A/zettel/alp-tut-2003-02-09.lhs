=============================================================================
ALP1-Tutorium 2003-02-06 / 2003-02-07
=============================================================================

Das ist der Baum wie in der Vorlesung

> data Eq a => Baum a = Leer | Knoten a [Baum a] deriving Eq


Das ist der Donnerstag

> blaetter1 :: Eq a => Baum a -> [a]
> blaetter1 Leer          = []
> blaetter1 (Knoten x xs) | istBlatt (Knoten x xs) = [x]
>                         | otherwise              = concat (map blaetter1 xs)

> istBlatt :: Eq a => Baum a -> Bool
> istBlatt (Knoten _ [])  = True
> istBlatt (Knoten  _ xs) = all (== Leer) xs
> istBlatt Leer           = False


Das ist der Freitag

> blaetter2 :: Eq a => Baum a -> [a]
> blaetter2 Leer          = []
> blaetter2 (Knoten x []) = [x]
> blaetter2 (Knoten x xs) | all (== Leer) xs = [x]
>                         | otherwise        = concat (map blaetter2 xs)


Hier noch Bäume zum Testen.
Aufruf zB mit > istBlatt t0

> t0 = Leer

> t1 = Knoten 1 []

> t2 = Knoten 5 [Knoten 2 [],
>                Knoten 7 
>                        [Knoten 6 [],
>                         Knoten 8 []
>                        ]
>               ]

> t3 = Knoten "Elizabeth" 
>                       [ Knoten "Charles" 
>                                        [Knoten "William" [],
>                                         Knoten "Henry"   []
>                                        ],                 
>                         Knoten "Andrew" 
>                                       [Knoten "Beatrice"  [],
>                                         Knoten "Eugenie"   []
>                                        ],
>                         Knoten "Edward" [],
>                         Knoten "Anne"
>                                        [Knoten "Peter"  [],
>                                         Knoten "Zara"   []
>                                        ] 
>                        ] 
