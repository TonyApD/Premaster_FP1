> {-# LANGUAGE UnicodeSyntax #-}
> module Char
> where
> import Unicode
> import Data.Char

> equal      ∷ String -> String -> Bool
> equal m n = map toLower m == map toLower n

> isNumeral  ∷ String → Bool
> isNumeral s = all isDigit s

> isBlank    ∷ String → Bool
> isBlank s = s == ""

> fromDigit  ∷ Char → Int
> fromDigit x
>     | isDigit x = ord x - 48

> toDigit    ∷ Int → Char
> toDigit x = chr (x + 48)

> shift      ∷ Int -> Char -> Char
> shift i c
>  | c == ' ' = ' '
>  | ord c + i <= 90 = chr(ord c + i)
>  | otherwise = chr(ord c + i - 26)

> msg  ∷  String
> msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
>         \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"
