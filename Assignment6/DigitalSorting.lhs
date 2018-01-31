> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE FlexibleInstances #-}
> module DigitalSorting
> where
> import Unicode
> import Prelude
> import Data.List (groupBy, sortBy)



---------------------------------------------------------------------------------
Not an exercise but useful for understanding the sorting problem and its efficiency.

> genericSort ∷ (Ord key) ⇒ [(key, val)] → [val]
> genericSort kvs  =  map snd (sortBy (\ kv1 kv2 → compare (fst kv1) (fst kv2)) kvs)

---------------------------------------------------------------------------------



---------------------------------------------------------------------------------
-- 6.6.1 & 6.6.2
-- As described in the exercise, we define the methods in the class Rank key as below.
-- We then create different instances for each type that we want to sort or rank... provided that that type also has
-- an instance of Ord (which we could implement ourself like the example below).
--    Example:
--        instance Ord () where
--        compare () () = EQ
--    This would also require a unique instance of the Rank class:
--        instance Rank () where
--        sort kvs = map snd kvs
--        rank kvs = [map snd kvs | not (null kvs)]
---------------------------------------------------------------------------------

> class Rank key where
>   sort  	 :: [(key, val)] → [val]
>   sort  	 =  concat ∘ rank
>   rank  	 :: [(key, val)] → [[val]]
>   listCompare :: [key]  → [key] → Ordering
>   compare2 :: key  → key → Ordering
>   compare2 k1 k2  -- compare expressed in terms of sort
> 			| head ( sort [(k1,1),(k2,2)]) == 1 = LT
> 			| head ( sort [(k1,1),(k2,2)]) == 2 = GT
>			| otherwise = EQ


--Examples of Rank implementations for specific types with group by and order by

> instance Rank Int where
>   listCompare = listCompare
>   rank kvs = map (map snd)
>				(groupBy (\g1 g2 ->  (fst g1) == (fst g2)  )
>				(sortBy (\kv1 kv2 -> compare (fst kv1) (fst kv2) ) kvs))

 instance Rank [Char] where
   rank kvs = map (map snd)
				(groupBy (\g1 g2 ->  (fst g1) == (fst g2)  )
				(sortBy (\kv1 kv2 -> compare (fst kv1) (fst kv2) ) kvs))


---------------------------------------------------------------------------------
6.6.3 -- We have defined a so-called lexicographic LIKE compare function that only takes the
-- second arguments in consideration then the first are equal.
-- For example:  compare2 (1::Int, 2::Int)(1::Int, 3::Int) evaluates compare 1 and 1 =EQ and thus compare 2 and 3 =LT

> instance (Rank key1, Rank key2) => Rank (key1, key2) where
>   rank = rank
>   listCompare = listCompare
>   compare2 (k11, k12) (k21, k22)
>           | compare2 k11 k21 == LT = LT
>           | compare2 k11 k21 == GT = GT
>			| otherwise = compare2 k12 k22

---------------------------------------------------------------------------------
6.6.4 -- We now define sum orderings as an Ranking instance comparing again keys.

> instance (Rank key1,Rank key2) ⇒ Rank (Either key1 key2) where
>   rank = rank
>   listCompare = listCompare
>   compare2 (Left a1)  (Left a2)  = compare2 a1 a2
>   compare2 (Left a1)  (Right b2) = LT
>   compare2 (Right b1) (Left a2)  = GT
>   compare2 (Right b1) (Right b2) = compare2 b1 b2


---------------------------------------------------------------------------------
6.6.5 -- We have defined a function to compare Rank lists as follows.

> instance (Rank key) ⇒ Rank [key] where
>   rank = rank
>   listCompare [] [] 	 = EQ
>   listCompare [] (_:_) = LT
>   listCompare (_:_) [] = GT
>   listCompare (a:as) (b:bs)
>                   | compare2 a b == EQ = listCompare as bs
>                   | otherwise = compare2 a b


---------------------------------------------------------------------------------
6.6.6 -- Missing repeatedSegments

< repeatedSegments :: (Rank key) ⇒ Int → [key] → [[Integer]]

< instance Rank Base where