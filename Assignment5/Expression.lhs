> {-# LANGUAGE UnicodeSyntax #-}
> module Expression
> where
> import Prelude hiding (fail)
> import Unicode ()
> import Parser

> infixl 6 :+:
> infixl 7 :*:
>
> data Expr
>   =  Lit Integer    -- a literal
>   |  Expr :+: Expr  -- addition
>   |  Expr :*: Expr  -- multiplication
>   deriving (Show)

An expression parser using >>=.

 expr, term, factor :: Parser Expr
 expr    =   term
         .|  (term >>= \ i → symbol '+' >> expr >>= \ j → return (i :+: j))
 term    =   factor
         .|  (factor >>= \ i → symbol '*' >> term >>= \ j → return (i :*: j))
 factor  =   (many1 digit >>= \ ds → return (Lit (read ds)))
         .|  (symbol '(' >> expr >>= \ i → symbol ')' >> return i)

Using do-notation.

 expr', term', factor' :: Parser Expr
 expr'    =   do term'
          .|  do i ← term' ; symbol '+' ; j ← expr' ; return (i :+: j)
 term'    =   do factor'
          .|  do i ← factor' ; symbol '*' ; j ← term' ; return (i :*: j)
 factor'  =   do ds ← many1 digit ; return (Lit (read ds))
          .|  do symbol '(' ; i ← expr' ; symbol ')' ; return i

parse expr "4*71+1"
parse expr "4 * 71 + 1"



-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
5.3.1

expr ::= (digit {digit} 
		| expr '+' expr 
		| expr '*' expr 
		| '(' expr ')')



> expr, lit :: Parser Expr
> expr  =  do lit
>		.| do i ← lit ; symbol '+' ; j ← expr ; return (i :+: j)
> 		.| do i ← lit ; symbol '*' ; j ← expr ; return (i :*: j)
> lit   =  do ds ← many1 digit ; return (Lit (read ds))  
> 	    .| do symbol '(' ; i ← expr ; symbol ')' ; return i

The biggest difference between the grammars is that the new grammar (implmentet
above), is more abstact, an expression can be a number or a operation consisting
of two expressions. While the other grammar defined seperate concepts for 
addition (term) and multiplication (factor).
Parsing simple equations like: "4*71+1", hava as difference that in the new grammar
there are added parenthesis









-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
5.3.2

Reconsider the expression grammar given in the lectures:
	expr ::= term
		| term ’+’ expr
	term ::= factor
		| factor ’*’ term
	factor ::= digit {digit}
		| ’(’ expr ’)’
Observe that the alternatives for expr and term share a commonprefix. 
An important optimization is to left factor a grammar to avoid repetitive parses e.g.
	expr ::= term (<empty word> | ’+’ expr)
	term ::= factor (<empty word> | ’*’ term)

This piece of code needs to be adapted to the left factored grammar:
(The hard part is to adapt the semantic actions.)
......

> expr', term', factor' :: Parser Expr
> expr'    =   do term'
>          .|  do i ← term' ; symbol '+' ; j ← expr' ; return (i :+: j)
> term'    =   do factor'
>          .|  do i ← factor' ; symbol '*' ; j ← term' ; return (i :*: j)
> factor'  =   do ds ← many1 digit ; return (Lit (read ds))
>          .|  do symbol '(' ; i ← expr' ; symbol ')' ; return i

What happens in tems of runningtime if the code is addapted?
(Within GHCi type :set +s to ask GHCi to print timing and 
memory statistics after each evaluation.)
....





