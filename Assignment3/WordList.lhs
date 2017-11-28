> {-# LANGUAGE UnicodeSyntax #-}
> module WordList
> where
> import Prelude hiding (Word)
> import Unicode
> import Data.List

> type Word  =  String

> lorem ∷ String
> lorem
>   = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
>     \nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam \
>     \erat, sed diam voluptua. At vero eos et accusam et justo duo dolores \
>     \et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
>     \Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur \
>     \sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et \
>     \dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam \
>     \et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea \
>     \takimata sanctus est Lorem ipsum dolor sit amet."

> wordList ∷ String → [(Word, Int)]
> wordList (s) = map(\w -> Word w, 1) words s

You may find the following library functions useful (in alphabetical order): filter, group, head, length, map, sort, sortOn, words.
