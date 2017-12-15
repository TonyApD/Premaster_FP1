 foldr:: (a -> ans -> ans) -> ans -> ([a] -> ans)

> allTrue = foldr (\x ans -> x && ans) True

> member x = foldr (\a ans -> x == a || ans) False

> smallest = foldr(\a ans -> a `min` ans) maxBound
