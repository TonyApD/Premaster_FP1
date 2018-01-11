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

% > foldm ∷ (a → a → a) → a → ([a] → a)
% > foldm (•) (x:[]) = x
% > foldm (•) (x:xs) = foldm (•) first • foldm (•) second
% >   where first = fst halved
% >         second = snd halved
% >         halved = splitAt ((length (x:xs)) `div` 2) (x:xs)

> kpg ∷ (Bit, Bit) → (Carry → Carry)
> kpg (O,  O  )  =  \ _c  → O  -- kill
> kpg (O,  I  )  =  \ c   → c  -- propagate
> kpg (I,  O  )  =  \ c   → c  -- propagate
> kpg (I,  I  )  =  \ _c  → I  -- generate

> data KPG  =  K | P | G

> data Additive = Sum {fromSum :: Int}
>   deriving (Show)
> instance Monoid Additive where
>   ε       = Sum 0
>   x • y   = Sum (fromSum x + fromSum y)

Exercise 6.2.1:

> data And = A Bool
>   deriving (Show)
> instance Monoid And where
>   ε       = A True
>   A x • A y   = A (x && y)

> data Or = B Bool
>   deriving (Show)
> instance Monoid Or where
>   ε       = B True
>   B x • B y   = B (x || y)

> data XOR = C Bool
>   deriving (Show)
> instance Monoid XOR where
>   ε       = C True
>   C x • C y   = C (x /= y)

> data Equal = D Bool
>   deriving (Show)
> instance Monoid Equal where
>   ε       = D True
>   D x • D y   = D (x == y)

> data FALSE = E Bool
>   deriving (Show)
> instance Monoid FALSE where
>   ε       = E True
>   E x • E y   = E (not x && not y)


•
Monoid is associative and has an identity element
