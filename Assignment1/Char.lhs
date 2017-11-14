> {-# LANGUAGE UnicodeSyntax #-}
> module Char
> where
> import Unicode
> import Data.Char

equal      ∷ String → String → Bool
isNumeral  ∷ String → Bool
isBlank    ∷ String → Bool
fromDigit  ∷ Char → Int
toDigit    ∷ Int → Char
shift      ∷ Int -> Char -> Char

> msg  ∷  String
> msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
>         \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"
