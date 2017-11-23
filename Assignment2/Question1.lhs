nott :: Bool -> Bool
nott False = True
nott True -> False

false :: Bool -> Bool
false False = False
false True = False

f2 :: (Bool, Bool) -> Bool
f2 (False, False) = False
f2 (False, True) =  True

f2 (False, b) =  b
f2 (True, b) =  not b

f2 (True, False) = True
f2 (True, True) = False

f2 (a, b) = a/= b
