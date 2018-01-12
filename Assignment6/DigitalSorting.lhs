> {-# LANGUAGE UnicodeSyntax #-}
> module DigitalSorting
> where
> import Unicode
> import Data.List (groupBy, sortBy)

> class (Ord key) => Rank key where
>   sort  	 :: [(key, val)] → [val]
>   rank  	 :: [(key, val)] → [[val]]
>   compare2 :: key  → key → Ordering
>   lexiSort :: [key] -> [key] -> Ordering

>   sort  	 =  concat ∘ rank
>   rank kvs = map (map snd) 
>				(groupBy (\g1 g2 -> (fst g1) == (fst g2)) 
>				(sortBy (\kv1 kv2 -> compare2 (fst kv1) (fst kv2) ) kvs))
>   compare2 k1 k2 
> 			| head (rank [(k1,k1),(k2,k2)]) == [k1] = GT
> 			| head (rank [(k1,k1),(k2,k2)]) == [k2] = LT
>			| otherwise = EQ
>   lexiSort k1 k2
>           | head k1 > head k2 = GT
>           | head k1 < head k2 = LT
>           | null k1 && null k2 = EQ
>           | otherwise = lexiSort (tail k1) (tail k2)


> genericSort ∷ (Ord key) ⇒ [(key, val)] → [val]
> genericSort kvs  =  map snd (sortBy (\ kv1 kv2 → compare (fst kv1) (fst kv2)) kvs)
 

> instance (Rank key1, Rank key2) => Rank (key1, key2) where
>   rank kvs = map (map snd) 
>				(groupBy (\g1 g2 -> (fst g1) == (fst g2)) 
>				(sortBy (\kv1 kv2 -> compare2 (fst kv1) (fst kv2) ) kvs))


> instance (Rank a1, Rank a2) ⇒ Rank (Either a1 a2) where
> 	compare2 (Left a1) (Left a2)   = compare2 a1 a2
> 	compare2 (Left a1) (Right b2)  = LT
> 	compare2 (Right b1) (Left a2)  = GT
> 	compare2 (Right b1) (Right b2) = compare2 b1 b2


> type List elem  =  Either () (elem, [elem])
>
> toList ∷ [elem] → List elem
> toList []        =  Left ()
> toList (a : as)  =  Right (a, as)

> instance (Rank key) ⇒ Rank [key] where
>   compare2 [] [] 	  = EQ
>   compare2 [] (_:_) = LT
>   compare2 (_:_) [] = GT
>   compare2 (a:as) (b:bs)
>                   | a > b = GT
>                   | a < b = LT
>                   | otherwise = EQ

