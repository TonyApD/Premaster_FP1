Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792


> {-# LANGUAGE UnicodeSyntax #-}
> module BinaryTree
> where
> import Unicode

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> instance (Eq elem) ⇒ Eq (Tree elem) where
> 	Empty 			== Empty 			= True
> 	(Node l e r) 	== Empty			= False
> 	Empty 			== (Node l e r)		= False
> 	(Node l1 e1 r1) == (Node l2 e2 r2)	= l1==l2 && e1==e2 && r1==r2

> instance (Ord elem) ⇒ Ord (Tree elem) where 
> 	Empty 			<= Empty 			= True
> 	(Node l e r) 	<= Empty			= False
> 	Empty 			<= (Node l e r)		= True
> 	(Node l1 e1 r1) <= (Node l2 e2 r2)	= l1<=l2 && e1<=e2 && r1<=r2


> ex1  ∷  Tree Integer
> ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
> ex2  ∷  Tree Integer
> ex2  =  Node Empty 4711 (Node Empty 0815 (Node Empty 43 Empty))
> ex3  ∷  Tree Integer
> ex3  =  Node Empty 4711 Empty
