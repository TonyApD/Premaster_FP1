Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> {-# LANGUAGE UnicodeSyntax #-}


From the docs:
 foldr:: (a -> ans -> ans) -> ans -> ([a] -> ans)

> allTrue :: [Bool] -> Bool
> allTrue l = foldr (&&) True l

> allFalse :: [Bool] -> Bool
> allFalse l = foldl (\x y -> x && (y == False) ) True l

> member :: (Eq a) ⇒ a -> [a] -> Bool
> member x l = foldr (\a ans -> x == a || ans) False l

> smallest :: [Int] -> Int
> smallest l = foldr (\a ans -> min a ans) maxBound l

> largest :: [Int] -> Int
> largest l = foldr (\a ans -> max a ans) minBound l


If both (folding and Exercise 3.2) recursion schemes are applicable,
which one is preferable in terms of running time?
I don't think that there is any difference, both functions perform the same operations,
It could be the compiler does somthing smart with them. But folding against the
recursion scheme form exercise 3.2 does not make a difference.
