> {-# LANGUAGE UnicodeSyntax #-}
> module RedBlackTree
> where
> import Unicode


> data RedBlackTree elem
>  = Leaf
>  | Red (RedBlackTree elem) elem (RedBlackTree elem)
>  | Black (RedBlackTree elem) elem (RedBlackTree elem)
>	deriving (Show)


> roodZwartBoom ∷  RedBlackTree Char
> roodZwartBoom =  Black (Red Leaf 'a' Leaf) 'b' (Black Leaf 'c' (Red Leaf 'd' (Black Leaf 'e' Leaf)))

> member ∷ (Ord elem) ⇒ elem → RedBlackTree elem → Bool
> member x Leaf          = False 
> member x (Red   l a r) = x == a || if x<a then member x l else member x r
> member x (Black l a r) = x == a || if x<a then member x l else member x r




Always insert a red node into the tree so only red constrains are violated...

> insert :: (Ord elem) ⇒ elem → RedBlackTree elem → RedBlackTree elem
> insert x Leaf          = Red Leaf x Leaf
> insert x (Red   l a r) | x < a     = (Red   (insert x l) a r) 
>                        | otherwise = (Red   l a (insert x r))
> insert x (Black l a r) | x < a     = (Black (insert x l) a r) 
>                        | otherwise = (Black l a (insert x r))


The constraints that can be violated are:

     B           B          B                B
    / \         / \        / \                \
   R           R              R                R
  / \         / \            / \              /
 R               R              R            R
/\              / \            / \          / \


Deze smart consturctor moet de situaties hierboven fixen...

 black :: RedBlackTree elem → elem → RedBlackTree elem → RedBlackTree elem


> isRed :: RedBlackTree elem -> Bool
> isRed (Red _ _ _) = True
> isRed _ 	      = False



