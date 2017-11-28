Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> allTrue :: [Bool] -> Bool
> allTrue []      = True
> allTrue (x:xs)  = x == True && allTrue xs == True

> allFalse :: [Bool] -> Bool
> allFalse []      = True
> allFalse (x:xs)  = x == False && allFalse xs == True

> member :: (Eq a) => a -> [a] -> Bool
> member (_)[]    = False
> member (a)(x:xs)  = a == x || member a xs == True

> smallest :: [Int] -> Int
> smallest []     = 100000
> smallest (x:xs) = minCompare x (smallest xs)

> largest :: [Int] -> Int
> largest []     = 0
> largest (x:xs) = maxCompare x (largest xs)

Helper functions for the min and max element calculations

> minCompare :: Int -> Int -> Int
> minCompare a b
>    | a > b  = b
>    | a <= b  = a

> maxCompare :: Int -> Int -> Int
> maxCompare a b
>    | a < b  = b
>    | a >= b  = a
