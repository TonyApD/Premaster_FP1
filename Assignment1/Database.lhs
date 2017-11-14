> {-# LANGUAGE UnicodeSyntax #-}
> module Database
> where
> import Unicode

> type Person  =  (Name, Age, FavouriteCourse)
>
> type Name             =  String
> type Age              =  Integer
> type FavouriteCourse  =  String

> frits, peter, ralf ∷ Person
> frits  =  ("Frits",  33,  "Algorithms and Data Structures")
> peter  =  ("Peter",  57,  "Imperative Programming")
> ralf   =  ("Ralf",   33,  "Functional Programming")

> students   ∷  [Person]
> students   =  [frits, peter, ralf]

> age ∷ Person → Age
> age (_n, a, _c)  =  a

name             ∷ Person → Name
favouriteCourse  ∷ Person → FavouriteCourse
showPerson       ∷ Person → String
twins            ∷ Person → Person → Bool
increaseAge      ∷ Person → Person
