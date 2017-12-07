> {-# LANGUAGE UnicodeSyntax #-}
> module WordList
> where
> import Prelude hiding (Word)
> import Unicode
> import Data.List
> import Data.Char (isAlphaNum, isSpace, toLower)
> import Data.Map (Map, fromListWith)


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
>     \takimata sanctus est Lorem ipsum dolor sit amet is."

> wordList :: String -> Map Word Int
> wordList s = fromListWith (+) $ zip (tokenize s) (repeat 1)

> tokenize :: Word -> [Word]
> tokenize s = words $ map toLower $ replace s

> replace :: Word -> Word
> replace s = [if isAllowed c then c else ' ' | c <- s]

> isAllowed :: Char -> Bool
> isAllowed c = isAlphaNum c || isSpace c
