Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

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

Next assignment 3.6

> loremConcat =
>   concat
>     [ "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
>     \nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam \
>     \erat, sed diam voluptua. At vero eos et accusam et justo duo dolores \
>     \et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
>     \Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur \
>     \sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et \
>     \dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam \
>     \et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea \
>     \takimata sanctus est Lorem ipsum dolor sit amet is."
>     ]

> format maxlen = wrap_ 0 . words
>   where
>     wrap_ _ [] = "\n"
>     wrap_ pos (w:ws)
>       | pos == 0 = w ++ wrap_ (pos + lw) ws
>       | pos + lw + 1 > maxlen = '\n' : wrap_ 0 (w : ws)
>       | otherwise = ' ' : w ++ wrap_ (pos + lw + 1) ws
>       where
>         lw = length w
