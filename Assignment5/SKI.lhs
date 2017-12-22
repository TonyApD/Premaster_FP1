> {-# LANGUAGE UnicodeSyntax #-}
> module SKI
> where
> import Unicode
> import Lambda

> data SKI var
>   =  Free var                 -- free/unbound variable
>   |  S
>   |  K
>   |  I'
>   |  App (SKI var) (SKI var)  -- application

> i ∷ env → env
> i arg = arg
> k ∷ a → (env → a)
> k x _arg = x
> s ∷ (env → a → b) → (env → a) → (env → b)
> s x y arg = (x arg) (y arg)

> abstr   ∷ (Eq var) ⇒ var → SKI var → SKI var
> abstr x (Free y)
>      | x == y         = I'
>      | otherwise      = App K (Free y)
> abstr x (App e1 e2)   = S `App` (abstr x e1) `App` (abstr x e2)
> abstr x e             = App K e

> compile ∷ (Eq var) ⇒ Lambda var → SKI var
> compile (Var x)       = Free x
> compile (Fun x e)     = abstr x (compile e)
> compile (e1 :@ e2)    = compile e1 `App` compile e2

> reduce  ∷ SKI var → [SKI var] → SKI var
> reduce (Free y)[]       = App K (Free y)
> reduce (Free y) (x:xs)  = App x (reduce x xs)


twice = parse expr "λf.λx.f(fx)"
twice
compile twice
reduce it [Free 's', Free 'z']

compile (twice :@ twice)
reduce it [Free 's', Free 'z']

parse expr "(λx.xx)(λx.xx)"
compile it
reduce it []
parse expr "λf.(λx.f(xx))(λx.f(xx))"
compile it
reduce it [Free 's', Free 'z']
