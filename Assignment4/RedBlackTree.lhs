Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792




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
> roodZwartBoom =  Black (Red Leaf 'a' Leaf) 'b' (Red Leaf 'c' Leaf)

> member ∷ (Ord elem) ⇒ elem → RedBlackTree elem → Bool
> member x Leaf          = False 
> member x (Red   l a r) = x == a || if x<a then member x l else member x r
> member x (Black l a r) = x == a || if x<a then member x l else member x r




Always insert a red node into the tree so only red constrains are violated...


This is the first insert function:

 insert :: (Ord elem) ⇒ elem → RedBlackTree elem → RedBlackTree elem
 insert x Leaf          = Red Leaf x Leaf
 insert x (Red   l a r) | x < a     = (Red   (insert x l) a r) 
                        | otherwise = (Red   l a (insert x r))
 insert x (Black l a r) | x < a     = (Black (insert x l) a r) 
                        | otherwise = (Black l a (insert x r))



The constraints that can be violated are:

     B           B          B                B
    / \         / \        / \                \
   R           R              R                R
  / \         / \            / \              /
 R               R              R            R
/\              / \            / \          / \


Door gebruik te maken van de smart constructor black (zie onder de insert methode)
kan deze methoden de red-black eigenschappen van de boom waarborgen. 

> insert        	:: (Ord elem) ⇒ elem → RedBlackTree elem → RedBlackTree elem
> insert a t 		= blacken (ins t)
>   where
>   ins Leaf		= (Red Leaf a Leaf)
>   ins (Red l b r )
> 	  | a < b 		= black (ins l) b r
> 	  | a == b 		= (Red l a r)
> 	  | a > b 		= black l b (ins r)
>   ins (Black l b r )
> 	  | a < b 		= black (ins l) b r
> 	  | a == b 		= Black l a r
> 	  | a > b 		= black l b (ins r )


> black :: RedBlackTree elem → elem → RedBlackTree elem → RedBlackTree elem
> black (Red (Red t1 a1 t2) a2 t3) a3 t4
>   = Red (Black t1 a1 t2) a2 (Black t3 a3 t4)
> black (Red t1 a1 (Red t2 a2 t3)) a3 t4
>   = Red (Black t1 a1 t2) a2 (Black t3 a3 t4)
> black t1 a1 (Red (Red t2 a2 t3) a3 t4)
>   = Red (Black t1 a1 t2) a2 (Black t3 a3 t4)
> black t1 a1 (Red t2 a2 (Red t3 a3 t4))
>   = Red (Black t1 a1 t2) a2 (Black t3 a3 t4)
> black l a r = (Black l a r)

> blacken :: RedBlackTree elem -> RedBlackTree elem 
> blacken (Red l a r)   = (Black l a r)
> blacken (Black l a r) = (Black l a r)


> isRedBlackTree :: RedBlackTree elem → Bool
> isRedBlackTree Leaf  					= True
> isRedBlackTree (Black l a r) = if blackheight l == blackheight r 
>									&& isRedBlackTree l && isRedBlackTree r
>								  then True 
>								  else False
> isRedBlackTree (Red l a r)   = if blackheight l == blackheight r 
>									&& isRedBlackTree l && isRedBlackTree r
>                                   && not (isRed l) && not (isRed r)       -- check for red violations
>								  then True 
>								  else False

> isRed :: RedBlackTree elem -> Bool
> isRed (Red l a r) = True
> isRed _ 			= False

> blackheight :: RedBlackTree elem -> Int
> blackheight Leaf 			= 0
> blackheight (Red l a r)   = (blackheight l `max` blackheight r)
> blackheight (Black l a r) = (blackheight l `max` blackheight r) + 1

