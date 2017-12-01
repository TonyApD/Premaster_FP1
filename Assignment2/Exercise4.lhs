> module Exercise4
> where
> import Data.Tuple

Define a function that swaps the two components of a pair.

> swap1 :: (Int, Int) -> (Int, Int)
> swap1 (a, b) = (b, a)

Define two other functions of this type (be inventive).

> swap2 :: (Int, Int) -> (Int, Int)
> swap2 = (\(a,b) -> (b,a))

And another one with the use of Data.Tuple

> swap3 :: (Int, Int) -> (Int, Int)
> swap3 = (\(a,b) -> swap (a,b))

Now swapping (HAHAHA) the definition to swap :: (a, b) → (b, a)

> swap4 :: (a, b) -> (b, a)
> swap4 (a, b) = (b, a)

Which works perfectly

> swap5 :: (a, b) -> (b, a)
> swap5 = (\(a,b) -> (b,a))

And the last one also works perfectly

> swap6 :: (a, b) -> (b, a)
> swap6 = (\(a,b) -> swap (a,b))
