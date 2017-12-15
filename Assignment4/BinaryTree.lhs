> {-# LANGUAGE UnicodeSyntax #-}
> module BinaryTree
> where
> import Unicode

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> instance Functor Tree where
>   fmap _f Empty         =  Empty
>   fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

preorder, inorder, postorder ∷ Tree elem → [elem]
layout ∷ (Show elem) => Tree elem → String
build ∷ [elem] → Tree elem
balanced ∷ [elem] → Tree elem
create ∷ Int → Tree ()

Node (Node Empty `a` Empty) `b` Empty :: Tree Char

> tree :: (Integer, Integer) -> Tree Integer
> tree (l, r)
>       | l > r     = Empty
>       | otherwise = Node (tree (l, r - 1)) r Empty

Exercise 4.1

> ex1  ∷  Tree Char
> ex1  =  Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty))

Exercise 4.2
I'm, asuming this needs to be done as a visual representation?¿?
Therefore also done in the tex file.

> ex2  ∷  Tree Integer
> ex2  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
> ex3  ∷  Tree String
> ex3  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
> ex4  ∷  Tree Char
> ex4  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

Exercise 4.3

> size ∷ Tree elem → Int
> size Empty        = 0
> size (Node l a r) = size l + 1 + size r

Exercise 4.4

> minHeight,maxHeight ∷ Tree elem → Int
> minHeight Empty        = 0
> minHeight (Node l a r)            = (min (minHeight l) (minHeight r)) + 1
> maxHeight (Node l a r)            = 1 + (max (maxHeight l) (maxHeight r))

Exercise 4.6

> member ∷ (Eq elem) ⇒ elem → Tree elem → Bool
> member x Empty          = False
> member x (Node l a r)   = x == a || member x l || member x r
