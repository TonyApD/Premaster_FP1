> {-# LANGUAGE UnicodeSyntax #-}
> module Database
> where
> import Unicode

> type Person  =  (Name, Age, FavouriteCourse)
>
> type Name             =  String
> type Age              =  Integer
> type FavouriteCourse  =  String

> tony, carlo, marnix ∷ Person
> tony  =  ("Tony Lopar",  22,  "Functional Programming")
> carlo  =  ("Carlo Jessurun",  21,  "Value Oriented Programming")
> marnix   =  ("Marnix Dessing",   22,  "Functional Programming")

> students   ∷  [Person]
> students   =  [tony, carlo, marnix]

> age ∷ Person → Age
> age (_n, a, _c)  =  a

> name ∷ Person → Name
> name (_n, a, _c)  =  _n

> favouriteCourse ∷ Person → FavouriteCourse
> favouriteCourse (_n, a, _c)  =  _c

> showPerson :: Person → String
> showPerson (_n, a, _c)  =  _n ++ " " ++ (show a) ++ " " ++ _c

> twins ∷ Person → Person → Bool
> twins (_n, a, _c) (_n2, a2, _c2)
>   | a == a2 = True
>   | otherwise = False

> increaseAge ∷ Person → Person
> increaseAge (_n, a, _c) = (_n, (a + 1), _c)
