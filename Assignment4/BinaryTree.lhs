> {-# LANGUAGE UnicodeSyntax #-}
> module BinaryTree
> where
> import Unicode

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> instance Functor Tree where
>   fmap _f Empty         =  Empty
>   fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

> ex1  ∷  Tree Integer
> ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
> ex2  ∷  Tree String
> ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
> ex3  ∷  Tree Char
> ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

> size ∷ Tree elem → Int
> size Empty        = 0
> size (Node l a r) = size l + 1 + size r

% > minHeight, maxHeight ∷ Tree elem → Int
% > minHeight, maxHeight Empty        = 0
% > maxHeight (Node l a r)            = 1 + (height l `max` height r)
% > minHeight (Node l a r)            = 1 + (height l `min` height r)

> member ∷ (Eq elem) ⇒ elem → Tree elem → Bool
> member x Empty          = False
> member x (Node l a r)   = x == a || member x l || member x r

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

> ex4  ∷  Tree Char
> ex4  =  Node (Node Empty 't' Empty) 'a' (Node Empty 'u' Empty)

Exercise 4.2
I'm, asuming this needs to be done as a visual representation?¿?
Therefore done in the tex file.
