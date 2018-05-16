> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE FlexibleInstances #-}
> import Prelude

> iszero :: Integer -> Integer
> iszero 0 = 1
> iszero _ = 0

> tolist :: String -> [[Char]]
> tolist []     = []
> tolist (x:xs) = [x]:(tolist xs)

> fromlist :: [[a]] -> [a]
> fromlist []       = []
> fromlist (x:xs)   = x ++ fromlist xs

> colorlist :: [String] -> [(String, String)]
> colorlist []   = []
> colorlist [x]  = []
> colorlist (x:xs) = [(y, z) | y <- (x:xs), z <- (x:xs), y < z]

> multiplication :: [Integer] -> [(Integer, Integer, Integer)]
> multiplication [] = []
> multiplication (x:xs) = [(a, b, a * b) | a <- (x:xs), b <- (x:xs)]

> multiplication2 :: [Integer] -> [[Integer]]
> multiplication2 [] = []
> multiplication2 (x:xs) = [[a * b | b <- (x:xs)] | a <- (x:xs)]

> insertionSort :: (Ord a) => [a] -> [a]
> insertionSort []     = []
> insertionSort (x:xs) = insert x (insertionSort xs)

> insert :: (Ord a) => a -> [a] -> [a]
> insert x []     = [x]
> insert x (y:ys)
>     | x < y     = x:y:ys
>     | otherwise = y:(insert x ys)

< compress :: [[Integer]] -> [(Int, Integer)]
< compress (x:xs) = [(length (x:xs), head (x:xs))]

> runs :: (Ord a) => [a] -> [[a]]
> runs []       = []
> runs (x:xs)   = extend x (runs xs)

> extend :: (Ord a) => a -> [[a]] -> [[a]]
> extend x []     = [[x]]
> extend x (r:rs)
>   | x == head r = (x : r) : rs
>   | otherwise   = [x] : r : rs

> runstotuple :: [[a]] -> [(Int, a)]
> runstotuple [] = []
> runstotuple (x:xs) = [(length x, head x)] ++ runstotuple xs

> runtotuple :: [a] -> (Int, a)
> runtotuple [x] = (1, x)
> runtotuple (x:xs) = (length (x:xs), x)

> uncompress :: [(Int, a)] -> [a]
> uncompress []           = []
> uncompress ((x, y):xs)  = nitems x y ++ uncompress xs

> nitems :: Int -> a -> [a]
> nitems 0 x = []
> nitems n x = [x] ++ nitems (n - 1) x
