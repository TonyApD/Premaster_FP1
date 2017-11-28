> runs :: (Ord a) => [a] -> [[a]]
> runs []       = []
> runs (x:xs)   = extend x (runs xs)

> extend :: (Ord a) => a -> [[a]] -> [[a]]
> extend x []     = [[x]]
> extend x (r:rs)
>   | x <= head r = (x : r) : rs
>   | otherwise   = [x] : r : rs
