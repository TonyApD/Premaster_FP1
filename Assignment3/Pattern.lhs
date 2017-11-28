Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> allTrue :: [Bool] -> Bool
> allTrue []      = True
> allTrue (x:xs)  = x && allTrue xs

> allFalse :: [Bool] -> Bool
> allFalse []      = True
> allFalse (x:xs)  = not x && allFalse xs

> member :: (Eq a) => a -> [a] -> Bool
> member _ []    = False
> member a (x:xs)  = a == x || member a xs

> smallest :: [Int] -> Int
> smallest []     = maxBound :: Int
> smallest (x:xs) = x `min` (smallest xs)

> largest :: [Int] -> Int
> largest []     = 0
> largest (x:xs) = x `max` (largest xs)
