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

Exercise 6.2.1:

> newtype OR      = MakeOr      {fromBool::Bool} deriving (Show)
> newtype Unequal = MakeUnequal {fromBool2::Bool} deriving (Show)
> newtype AND     = MakeAnd     {fromBool3::Bool} deriving (Show)
> newtype Equal   = MakeEqual   {fromBool4::Bool} deriving (Show)

> instance Monoid OR where
>   ε       = MakeOr False
>   x • y   = MakeOr (fromBool x || fromBool y)

> instance Monoid AND where
>   ε       = MakeAnd True
>   x • y   = MakeAnd (fromBool3 x && fromBool3 y)

> instance Monoid Unequal where
>   ε       = MakeUnequal False
>   x • y   = MakeUnequal (fromBool2 x /= fromBool2 y)

> instance Monoid Equal where
>   ε       = MakeEqual True
>   x • y   = MakeEqual (fromBool4 x == fromBool4 y)

Exercise 6.3:

> instance (Ord elem) ⇒ Monoid (OrdList elem) where
>   ε                   = Ord []
>   (Ord x) • (Ord y)   = Ord (runs x y)

Exercise 6.4:
Top-down implementation of foldm

> foldm ∷ (a → a → a) → a → ([a] → a)
> foldm (•) ε []      = ε
> foldm (•) ε [x]     = (•) ε x
> foldm (•) ε list    = (foldm (•) ε first) • (foldm (•) ε second)
>   where first = fst halved
>         second = snd halved
>         halved = splitAt ((length list) `div` 2) list

Bottom-up implementation of foldm(called foldmb since foldm is already defined above)

> foldmb ∷ (a → a → a) → a → ([a] → a)
> foldmb (•) ε []       = ε
> foldmb (•) ε (x:[])   = (•) ε x
> foldmb (•) ε (x:y:[]) = (•) ε ((•) x y)
> foldmb (•) ε (x:y:xs) = foldmb (•) ε (xs ++ [((•) x y)])

> runs :: (Ord a) => [a] -> [a] -> [a]
> runs [][] = []
> runs [x][] = [x]
> runs [][y] = [y]
> runs (x:xs) (y:ys) = if x < y then x:runs xs (y:ys) else y:runs (x:xs) ys

Exercise 6.5

> kpg ∷ (Bit, Bit) → (Carry → Carry)
> kpg (O,  O  )  =  \c  → O  -- kill
> kpg (O,  I  )  =  \c   → c  -- propagate
> kpg (I,  O  )  =  \c   → c  -- propagate
> kpg (I,  I  )  =  \c  → I  -- generate

> data KPG  =  K | P | G
>     deriving(Show, Eq)

% > newtype KPG = KPG (Bit, Bit) deriving Show

> instance Monoid KPG where
>   ε           = P   -- Since P is the representation for the function id
>   x • y
>     | x == K  = K
>     | x == P  = y
>     | x == G  = G

> apply :: KPG -> (Carry -> Carry)
> apply P = id
> apply K = (\c ->  O)
> apply G = (\c ->  I)

> toKpg :: (Bit, Bit) → KPG
> toKpg (O,O) = K
> toKpg (O,I) = P
> toKpg (I,O) = P
> toKpg (I,I) = G
