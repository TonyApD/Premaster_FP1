Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> inverse :: Bool -> Bool
> inverse x = not x

> false :: Bool -> Bool
> false _ = False

> true :: Bool -> Bool
> true _ = True

> same :: Bool -> Bool
> same b = b

Or function

> for :: (Bool, Bool) -> Bool
> for (True, _) = True
> for (False, x) = x

And function

> fand :: (Bool, Bool) -> Bool
> fand (x, y) = x && y

XOR-function

> fxor :: (Bool, Bool) -> Bool
> fxor (x, y)
>     | x == y    = False
>     | otherwise = True

Equals function which returns whether the inputs are equal using the defined xor and inverse function

> equals :: (Bool, Bool) -> Bool
> equals (a, b)   = inverse (fxor(a, b))
