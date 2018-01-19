> {-# LANGUAGE UnicodeSyntax #-}
> module Text
> where
> import Unicode
> import BinaryTree

Interface.

> infixr 5 <>
> text    ∷ String → Text       -- without '\n'
> nl      ∷ Text
> indent  ∷ Int → Text → Text
> (<>)    ∷ Text → Text → Text
>
> render2  ∷ Text → String

Reference implementation.

> data Text  =  Text String      -- without '\n'
>            |  Nl
>            |  Indent Int Text
>            |  Text :<> Text
>            deriving (Show)
>
> text s = render2 (Text s)
> nl = render2 (Nl)
> indent i d = render2 (Indent i d)
> d1 <> d2 = render2 (d1 :<> d2)
>
> render2 (Text s)      =  s
> render2 (Nl)          =  "\n"
> render2 (Indent i d)  =  renderWith (Indent i d) i ""
> render2 (d1 :<> d2)   =  renderWith (d1 :<> d2) 0 ""

The helper function ``tab i s'' inserts ``i'' spaces after each
newline in ``s''.

> tab ∷ Int → String → String
> tab _i ""        =  ""
> tab i (c : s)
>     | c == '\n'  =  c : replicate i ' ' ++ tab i s
>     | otherwise  =  c :                    tab i s

Application.

> prettyTree ∷ (Show elem) ⇒ Tree elem → Text
> prettyTree Empty
>   =  text "Empty"
> prettyTree (Node l a r)
>   =  indent 4 (  text "Node"    <> nl <>
>                  prettyTree l   <> nl <>
>                  text (show a)  <> nl <>
>                  prettyTree r)

> atree ∷ Tree Integer
> atree
>   = Node (Node Empty 1 (Node Empty 2 Empty))
>        3 (Node (Node Empty 4 Empty) 5 (Node Empty 6 Empty))

prettyTree atree
render it
putStrLn it

Exercise 7.5

> renderWith:: Text -> Int -> String -> String
> renderWith (Text s) i x     = s ++ x
> renderWith (Nl) i x         = "\n" ++ (space i) ++ x
> renderWith (Indent j s) i x = renderWith s (i+j) x
> renderWith (d1 :<> d2) i x  = renderWith d1 i (renderWith d2 i x)

> space:: Int -> String
> space 1   = " "
> space i   = " " ++ (space (i - 1))

% > prettyText ∷ Text -> Text
% > prettyText Empty
% >   =  text "Empty"
% > prettyText (Node l a r)
% >   =  indent 4 (  text "Node"  <>  renderWith nl 1
% >                  (prettyText l   <> nl <>
% >                  text (show a)  <> nl <>
% >                  prettyText r))
