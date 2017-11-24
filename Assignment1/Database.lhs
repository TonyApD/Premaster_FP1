Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

> {-# LANGUAGE UnicodeSyntax #-}
> module Database
> where
> import Unicode

> type Person  =  (Name, Age, FavouriteCourse, NativeLanguage)
>
> type Name             =  String
> type Age              =  Integer
> type FavouriteCourse  =  String
> type NativeLanguage   =  String

> tony, carlo, marnix ∷ Person
> tony  =  ("Tony Lopar",  22,  "Functional Programming", "Ukraine")
> carlo  =  ("Carlo Jessurun",  21,  "Value Oriented Programming", "English")
> marnix   =  ("Marnix Dessing",   22,  "Functional Programming", "Dutch")

> students   ∷  [Person]
> students   =  [tony, carlo, marnix]

> age ∷ Person → Age
> age (_n, a, _c, _l)  =  a

> name ∷ Person → Name
> name (_n, a, _c, _l)  =  _n

> favouriteCourse ∷ Person → FavouriteCourse
> favouriteCourse (_n, a, _c, _l)  =  _c

> showPerson :: Person → String
> showPerson (_n, a, _c, _l)  =  _n ++ " " ++ (show a) ++ " " ++ _c

> twins ∷ Person → Person → Bool
> twins (_n, a, _c, _l) (_n2, a2, _c2, _l2)
>   | a == a2 = True
>   | otherwise = False

> increaseAge ∷ Person → Person
> increaseAge (_n, a, _c, _l) = (_n, (a + 1), _c, _l)
