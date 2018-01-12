Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> {-# LANGUAGE UnicodeSyntax #-}
> module Calculus
> where
> import Unicode

> data Primitive
>   =  Sin  -- trigonometric: sine
>   |  Exp  -- exponential
>   deriving (Show)
>
> infixl 6 :+:
>
> data Function
>   =  Const Rational         -- constant function
>   |  Id                     -- identity
>   |  Prim Primitive         -- primitive function
>   |  Function :+: Function  -- addition of functions
>   deriving (Show)

infixl 7 :*:
infixr 9 :.:

apply    ∷ Function → (Double → Double)

% > derive   ∷ Function → Function
% > derive (Const a)  = Const 0
% > derive (Id)       = Const 1
% > derive (f :+: g)  = derive f :+: derive g
% > derive (f :*: g)  = (derive f * g) :+: (f * derive g)
%
% > instance Num Function where
% > Const 0 * f = Const 0
% > f * Const 0 = Const 0
% > Const l * f = f
% > f *: Const 1 = f
% > f * g = f :*: g

simplify ∷ Function → Function
