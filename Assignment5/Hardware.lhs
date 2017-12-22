> {-# LANGUAGE UnicodeSyntax #-}
> module Hardware
> where
> import Numeral

> data Bit  =  O | I
>   deriving (Eq, Ord, Show)

> type BinaryInteger = [Bit]

> infixr 3 ∧
> (∧) ∷ Bit → Bit → Bit
> O ∧ _b  =  O
> I ∧ b   =  b

> infixr 2 ∨
> (∨) ∷ Bit → Bit → Bit
> O ∨ b   =  b
> I ∨ _b  =  I

> infixr 4 ⊕
> (⊕) ∷ Bit → Bit → Bit
> O ⊕ O  =  O
> O ⊕ I  =  I
> I ⊕ O  =  I
> I ⊕ I  =  O

< mapr ∷ ((a, state) → (b, state)) → (([a], state) → ([b], state))
< mapr ((a, sa)->(b, sb)) = scanr (⊕) 0 where (a, sa)⊕(b, sb) = ((a:[], sa)->(b:[], sb))

foldl (⊗) 0 where a ⊗ b = a * base + b

> type Carry  =  Bit

> halfAdder ∷ (Bit, Bit) → (Bit, Carry)
> halfAdder (a, b) = (a ⊕ b, a ∧ b)

> fullAdder ∷ ((Bit, Bit), Carry) → (Bit, Carry)
> fullAdder ((a, b), c) = (a ⊕ b ⊕ c, (((a ⊕ b) ∧ c) ∨ (a ∧ b)))
