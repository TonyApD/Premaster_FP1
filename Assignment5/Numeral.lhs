> {-# LANGUAGE UnicodeSyntax #-}
> module Numeral
> where
> import Unicode

> type Base   =  Integer
> type Digit  =  Integer

> msdf :: Base -> [Digit] -> Integer
> msdf base = foldl (⊗) 0 where a ⊗ b = a * base + b

> lsdf :: Base -> [Digit] -> Integer
> lsdf base = foldr (⊗) 0 where a ⊗ b = a + base * b

> reverseList :: [Digit] -> [Digit]
> reverseList [] = []
> reverseList (x:xs) = reverseList xs ++ [x]
