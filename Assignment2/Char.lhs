> {-# LANGUAGE UnicodeSyntax #-}
> module Char
> where
> import Unicode
> import Data.Char

> equal      ∷ (String -> String) -> Bool
> equal (m, n) = map (\String -> toLower m) == map (\String ->toLower n)

> isNumeral  ∷ String → Bool
> isNumeral (s) = all isDigit s

> isBlank    ∷ String → Bool
> isBlank s = s == ""

> fromDigit  ∷ Char → Int
> fromDigit x
>     | isDigit x = fromEnum x - fromEnum 'a'
>     | otherwise   = 0

 toDigit    ∷ Int → Char
 toDigit x = '' ++ x

 shift      ∷ Int -> Char -> Char
 shift (i, c)

> msg  ∷  String
> msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
>         \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"
