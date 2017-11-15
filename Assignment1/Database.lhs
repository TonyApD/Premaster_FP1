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
> tony  =  ("Tony Lopar",  42,  "Functional Programming")
> carlo  =  ("Carlo Jessurun",  42,  "Value Oriented Programming")
> marnix   =  ("Marnix Dessing",   42,  "Functional Programming")

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
> twins (_n, a, _c)
>   | a /= a = False
>   | otherwise =  True

> increaseAge ∷ Person → Person
> increaseAge (_n, a, _c) = p

name             ∷ Person → Name
favouriteCourse  ∷ Person → FavouriteCourse
showPerson       ∷ Person → String
twins            ∷ Person → Person → Bool
increaseAge      ∷ Person → Person
