Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> inverse :: Bool -> Bool
> inverse False = True
> inverse True = False

> false :: Bool -> Bool
> false b = False

> true :: Bool -> Bool
> true b = True

> same :: Bool -> Bool
> same b = b

Or-function

> for :: (Bool, Bool) -> Bool
> for (_, True) = True
> for (True,_) = True
> for (_, _) = False

And function

> fand :: (Bool, Bool) -> Bool
> fand (True, True) = True
> fand (_,_) = False

XOR-function

> fxor :: (Bool, Bool) -> Bool
> fxor (False, True)  = True
> fxor (True, False)  = True
> fxor (_, _)         =  False

Equals function which returns whether the inputs are equal using the defined xor and inverse function

> equals :: (Bool, Bool) -> Bool
> equals (a, b)   = inverse (fxor(a, b))
