> {-# LANGUAGE UnicodeSyntax #-}
> module BinarySearchTree
> where
> import Unicode
> import BinaryTree  -- hiding (member)
> import QuickTest

> registry  ∷  Tree String
> registry  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

> memberST ∷ (Ord elem) ⇒ elem → Tree elem → Bool
> memberST x Empty  = False
> memberST x (Node l a r)
>    | x == a       = True
>    | x <= a       = memberST x l
>    | x >= a       = memberST x r

> insert ∷ (Ord elem) ⇒ elem → Tree elem → Tree elem
> insert x Empty      = Node Empty x Empty
> insert x (Node l a r)
>       | x <= a      = Node (insert x l) a r
>       | x > a       = Node l a (insert x r)

> delete ∷ (Ord elem) ⇒ elem → Tree elem → Tree elem
> delete x Empty      = Empty
> delete x (Node Empty a Empty)
>     | x == a        = Empty
>     | otherwise     = (Node Empty a Empty)
> delete x (Node Empty a (Node rl ra rr))
>     | x == a        = (Node Empty ra rr)
>     | x > a         = (Node Empty a (delete x (Node rl ra rr)))
>     | otherwise     = (Node Empty a (Node rl ra rr))
> delete x (Node (Node ll la lr) a Empty)
>     | x == a        = (Node ll la Empty)
>     | x < a         = (Node (delete x (Node ll la lr)) a Empty)
>     | otherwise     = (Node (Node ll la lr) a Empty)
> delete x (Node l a r)
>     | x == a        = (Node l a r)
>     | x < a         = (Node (delete x l) a r)
>     | x > a         = (Node l a (delete x r))

> isSearchTree ∷ (Ord elem) ⇒ Tree elem → Bool
> isSearchTree Empty         = True
> isSearchTree (Node Empty a Empty)  = True
> isSearchTree (Node Empty a (Node rl ra r))  = a <= ra && isSearchTree r
> isSearchTree (Node (Node l la lr) a Empty)  = a >= la && isSearchTree l
> isSearchTree (Node (Node l la lr) a (Node rl ra r))  = a >= la && a <= ra && isSearchTree l && isSearchTree r

trees ∷ [elem] → Probes (Tree elem)  -- should be defined in BinaryTree


 search :: (Eq elem) => elem -> SearchTree elem -> bool
 search x Empty  = False
 search x (Node l a r)
     | x < a   = search x l
     | x == a  = True
     | x > a   = search x r

 insert ∷ (Ord elem) ⇒ elem → SearchTree elem → SearchTree elem
 insert x Empty      =
 insert x (Node l a r)
     | x < a   = Node (insert x l) a r
     | x == a  = l a r
     | x > a   = Node l a (insert x r)
