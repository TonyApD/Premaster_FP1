Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> {-# LANGUAGE UnicodeSyntax #-}
> module QuickTest (Probes, Property, (-->), (==>))
> where
> import Unicode
> import Data.List (sort)

> type Probes a    =  [a]
>
> type Property a  =  a → Bool

> infixr 1  -->, ==>
>
> (-->)   ∷ Probes a → Property b → Property (a → b)
> (==>)   ∷ Probes a → (a → Property b) → Property (a → b)
>
> probes --> prop  =  \ f → and [ prop (f x) | x ← probes ]
> probes ==> prop  =  \ f → and [ prop x (f x) | x ← probes ]

> ordered      ∷ (Ord a) ⇒ Property [a]
> ordered []   = True
> ordered  [_]  = True
> ordered (x:y:xs) = x <= y && ordered (y:xs)

> permutations ∷ [a] → Probes [a]
> permutations [] = [[]]
> permutations (x:xs) = [y | p <- permutations xs, y <- insert p]
>  where
>    insert []     = [[x]]
>    insert (y:ys) = (x:y:ys) : map (y:) (insert ys)

> isqrt ∷ Integer → Integer
> isqrt n = loop 0 3 1
>   where loop i k s  | s ≤ n      = loop (i + 1) (k + 2) (s + k)
>                     | otherwise  = i

< isIntegerSqrt :: Property (Integer -> Integer)
< isIntegerSqrt i = i --> isqrt i * isqrt i == i

> infixr 4  ⊗
> (⊗) ∷ Probes a → Probes b → Probes (a, b)
> (⊗) [] [] = []
> (⊗) a b = [ (x,y) | x<-a, y<-b ]

> niftySort ∷ [a] → [a]
> niftySort _xs  =  []
>
> trustedSort ∷ (Ord a) ⇒ [a] → [a]
> trustedSort  =  sort

> orderedrun      ∷ (Ord a) ⇒ Property [a]
> orderedrun []   = True
> orderedrun  [_]  = True
> orderedrun (x:y:xs) = x > y && ordered (y:xs)

> runs :: (Ord a) => [a] -> [[a]]
> runs []       = []
> runs (x:xs)   = extend x (runs xs)

> extend :: (Ord a) => a -> [[a]] -> [[a]]
> extend x []     = [[x]]
> extend x (r:rs)
>   | x <= head r = (x : r) : rs
>   | otherwise   = [x] : r : rs
