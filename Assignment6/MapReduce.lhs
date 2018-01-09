> {-# LANGUAGE UnicodeSyntax #-}
> module MapReduce
> where
> import Prelude hiding (Monoid)
> import Unicode
> import Hardware

> class Monoid a where
>   ε    ∷  a
>   (•)  ∷  a → a → a

> reduce  ∷  (Monoid m) ⇒ [m] → m
> reduce  =  foldr (•) ε

> newtype OrdList elem = Ord [elem]

instance (Ord elem) ⇒ Monoid (OrdList elem) where

> foldm ∷ (a → a → a) → a → ([a] → a)
> foldm (•) (x:[]) = x
> foldm (•) (x:xs) = foldm (•) first • foldm (•) second
>   where first = fst halved
>         second = snd halved
>         halved = splitAt ((length (x:xs)) `div` 2) (x:xs)

% > balanced ∷ [elem] → Tree elem
% > balanced [] = Empty
% > balanced l = Node (balanced first) (head second) (balanced (tail second))
% >   where first = fst halved
% >         second = snd halved
% >         halved = splitAt ((length l) `div` 2) l

> kpg ∷ (Bit, Bit) → (Carry → Carry)
> kpg (O,  O  )  =  \ _c  → O  -- kill
> kpg (O,  I  )  =  \ c   → c  -- propagate
> kpg (I,  O  )  =  \ c   → c  -- propagate
> kpg (I,  I  )  =  \ _c  → I  -- generate

> data KPG  =  K | P | G

% > newtype Additive = Sum {fromSum :: Int}
% >   deriving (Show)
% > instance Monoid Additive where
% > ε = Sum 0
% > x • y = Sum (fromSum x + fromSum y)


•
Monoid is associative and has an identity element
