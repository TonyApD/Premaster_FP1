>{-# LANGUAGE UnicodeSyntax #-}
>module Database
>where
>import Unicode


twice::x

>twice= \f -> \x -> f (f x)
>bla::Integer -> Bool
>bla x = (x > 12)