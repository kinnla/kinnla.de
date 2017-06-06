==========================================================================
ALP1 - ÜBUNG 12                                   Tutor: Till Zoppke
Nadine Alexander, Philipp Schmidt
==========================================================================


Task 1 - Rededfining Lists

> data Liste a = Leer | Cons (Liste a) a

an operator would e nice.... put it on same level than ++ and
make it left associative

> (#) = Cons
> infixl 5 #

--------------------------------------------------------------------------
cat - concat two lists
--------------------------------------------------------------------------

> cat :: (Liste a) -> (Liste a) -> (Liste a)
> cat Leer xs = xs 
> cat ys Leer = ys 
> cat ys (Cons xs x) = Cons (cat ys xs) x


--------------------------------------------------------------------------
mlength - get the length of a list
--------------------------------------------------------------------------

> mlength :: (Liste a) -> Int
> mlength Leer = 0
> mlength (Cons ls _) = (mlength ls) +1


--------------------------------------------------------------------------
ind - get the nth element of a list (from left)
--------------------------------------------------------------------------

> ind :: Int -> (Liste a) -> a
> ind n (Cons xs x) | mlength xs == n = x
>                   | mlength xs < n  = error "List too short"
>                   | otherwise       = ind n xs

--------------------------------------------------------------------------
mhead - get the first element of the list
--------------------------------------------------------------------------

> mhead :: (Liste a) -> a
> mhead (Cons Leer x) = x
> mhead (Cons xs   _) = (mhead xs)
> mhead Leer          = error "List too short"

--------------------------------------------------------------------------
mtake - take n elements from the left
--------------------------------------------------------------------------

> mtake :: Int -> (Liste a) -> (Liste a)
> mtake n (Cons xs x) | mlength xs == n = xs
>                     | mlength xs < n  = error "List too short"
>                     | otherwise       = mtake n xs

--------------------------------------------------------------------------
mmap - apply a function (a->b) to al list
--------------------------------------------------------------------------

> mmap :: (a->b) -> (Liste a) -> (Liste b)
> mmap _  Leer       = Leer
> mmap f (Cons xs x) = Cons (mmap f xs) (f x)


testruns:
        Hugs session for:
        Prelude.hs
        alp1-uez-2003-02-05.lhs
        Main> mlength (Leer#1#2#3#4#5)
        5
        (39 reductions, 42 cells)
        Main> mhead (Leer#1#2#3#4#5)
        1
        (17 reductions, 18 cells)
        Main> ind 3 (Leer#1#2#3#4#5)
        4
        (90 reductions, 93 cells)
        Main> mhead (mmap (+1) (Leer#1#2#3#4#5))
        2
        (28 reductions, 38 cells)
        Main> 

more testruns without a show function make no sense.... have phun





Task 2 - Fun with trees
--------------------------------------------------------------------------

first we define a tree... 

empty trees cause real Problems because a treewith empty trees as leaves
is eqal to a tree without leaves - who should implement that???


> data (Eq a) => Tree a = Node a [Tree a] deriving Eq

> 

--------------------------------------------------------------------------
root t - returns the description of the tree root
--------------------------------------------------------------------------

> root :: (Eq a) => Tree a -> a
> root (Node e _) = e



--------------------------------------------------------------------------
find  - checks weather a node with descr. a is in the tree
--------------------------------------------------------------------------

> find :: (Eq a) => a -> Tree a -> Bool
> find d (Node e []) = (d==e)   
> find d (Node e xs) = (d==e) || any (find d) xs 

--------------------------------------------------------------------------
parent  - returns the parent of a node with identifier a
--------------------------------------------------------------------------

> parent :: (Eq a) => a -> Tree a -> a
> parent s (Node e xs) | (s == e)       = error "the root is an orphan"  
>                      | (p == Nothing) = error "wrong tree - not even have the node"   
>                      | otherwise     = rp
>        where p = rparent s (Node e xs) 
>              Just rp = p
>              rparent :: (Eq a) => a -> Tree a -> Maybe a 
>              rparent _  (Node _  []) = Nothing  
>              rparent rs (Node re xs)
>                      | isparent rs xs = Just re
>                      | otherwise = head
>                          (filter (/= Nothing) (map (rparent rs) xs) ++  [Nothing])
>              isparent e = any (\(Node d _) -> (d == e))

--------------------------------------------------------------------------
children  - returns the children of a node with identifier a
--------------------------------------------------------------------------

> children :: (Eq a) => a -> Tree a -> [a]
> children _ (Node _ []) = []
> children s (Node e xs) | (s == e)  = map (\(Node d _) -> d) xs         
>                        | otherwise = concat (map (children s) xs)

--------------------------------------------------------------------------
depth - returns the depth of a node
--------------------------------------------------------------------------

> depth :: (Eq a) => a -> Tree a -> Int
> depth s (Node e xs) = (rdepth s (Node e xs))-1 
>         where rdepth s (Node e xs) | (e == s)  = 1
>                                    | (xs == [])= 0
>                                    | (cl >1 )  = cl 
>                                    | otherwise = 0
>                                   where cl = sum (1:(map (rdepth s) xs))


--------------------------------------------------------------------------
cousins - returns the cousins of degree i of x
--------------------------------------------------------------------------

> cousins :: (Eq a) => Int -> a -> Tree a -> [a]
> cousins i x =  (visitCousins i) .
>                (eleminateFamily x) . 
>                (findCommonParent (i+1) x )
>    where

        find out the common parent by wakling alog the tree

>       findCommonParent :: (Eq a) => Int -> a -> Tree a -> Tree a
>       findCommonParent i x t 
>                      | (isCommonParent i x t) = t
>                      | (ct /= []) = head ct
>                      | otherwise = error "who the fuck knows about generations far gone"
>                         where (Node _ st) = t 
>                               ct = (map (findCommonParent i x) st)
>                               isCommonParent i x t = (find x t) && 
>                                                      ((depth x t) == i)

        eleminate the branch of the given cousin, because he should not
        be his own cousin -  the eleminates also branches of cousins
        with the same name - lack of fantasy needs punishment

>       eleminateFamily :: (Eq a) => a -> Tree a -> Tree a
>       eleminateFamily x t = (Node d (filter (not.(find x)) st))
>                       where (Node d st) = t 

        visit the remaining cousins

>       visitCousins :: (Eq a) => Int -> Tree a -> [a]
>       visitCousins i t | i > 0  = concat (map (visitCousins (i-1)) st)
>                        | i == 0 = [nodeName n | n <- st] 
>                       where (Node _ st) = t
>                             nodeName = (\(Node n _) -> n)



Task 3
--------------------------------------------------------------------------
show instance Tree
--------------------------------------------------------------------------

> instance (Eq a, Show a) => Show (Tree a) where
>      show (Node l st) = drawTree (Node l st)


> drawTree :: (Eq a, Show a)  => Tree a -> String
> drawTree (Node l st) = (show l) ++ drawSubtrees st 0
>          where 
>              drawSubtrees t d = concat (["\n"
>                                ++(concat(replicate d " |  "))
>                                ++ " +--" ++ (show l)
>                                ++ (drawSubtrees st (d+1))
>                                | (Node l st) <- t ])

> demoTree = (Node "aaa" [(Node "baa" [(Node "bba" []),(Node "bab" [(Node "abb" [])])]),(Node "cbb" []), (Node "bcc" [(Node "bcb" []) ])
>            ])

testruns:

       Main> demoTree
       "aaa"
        +--"baa"
        |   +--"bba"
        |   +--"bab"
        |   |   +--"abb"
        +--"cbb"
        +--"bcc"
        |   +--"bcb"
       (1151 reductions, 2376 cells)
       Main> children "aaa" demoTree
       ["baa","cbb","bcc"]
       (230 reductions, 320 cells)
       Main> parent "bcb" demoTree
       "bcc"
       (259 reductions, 375 cells)
       Main> cousins 1 "bcb" demoTree
       ["bba","bab"]
       (635 reductions, 915 cells)
       Main> depth "bcb" demoTree
       2
       (222 reductions, 350 cells)
       Main> find "bcb" demoTree
       True
       (124 reductions, 181 cells)
       Main> 


translate it

> type Baum a = Tree a
> wurzel :: (Eq a) => Baum a -> a
> wurzel = root
> vater :: (Eq a) => a -> Baum a -> a
> vater = parent
> kinder :: (Eq a) => a -> Baum a -> [a]
> kinder = children
> tiefe :: (Eq a) => a -> Baum a -> Int
> tiefe = depth


