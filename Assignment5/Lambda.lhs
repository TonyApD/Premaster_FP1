> {-# LANGUAGE UnicodeSyntax #-}
> module Lambda
> where
> import Prelude hiding (fail)
> import Unicode ()
> import Text.Parsec
> import Text.Parsec.String

> data LambdaExpr = Variable Char
>                 | Abstraction Char LambdaExpr
>                 | Application LambdaExpr LambdaExpr
>                 deriving Show

> lambdaExprParser :: Parser LambdaExpr
> lambdaExprParser = do char '\\'
>                       var <- letter
>                       char '.'
>                       expr <- lambdaExprParser
>                       return $ Abstraction var expr
>                <|> do apps <- many1 term
>                       return $ foldl1 Application apps

> term :: Parser LambdaExpr
> term = do var <- letter
>           return $ Variable var
>    <|> do char '('
>           expr <- lambdaExprParser
>           char ')'
>           return expr
