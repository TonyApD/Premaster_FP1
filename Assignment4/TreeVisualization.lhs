> {-# LANGUAGE UnicodeSyntax #-}
> module TreeVisualization
> where
> import Unicode
> import Data.List

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> boom ∷  Tree Char
> boom =  Node (Node (Node Empty 'z' Empty) 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty) )

> preorder ∷ Tree elem → [elem]
> preorder Empty = []
> preorder (Node l v r)  = [v] ++ preorder l ++ preorder r

> inorder ∷ Tree elem → [elem]
> inorder Empty = []
> inorder (Node l v r)  = inorder l ++ [v] ++ inorder r

> postorder ∷ Tree elem → [elem]
> postorder Empty = []
> postorder (Node l v r)  = postorder l ++ postorder r ++ [v]

> layout :: (Show elem) ⇒ Tree elem → String
> layout Empty = "No tree to be displayed"
> layout (Node l v r) =
> 	(layoutSubTree l 1) ++ "  /" ++ "\n" ++
> 	"-"++(show v) ++ "\n" ++
>	"  \\ \n" ++ (layoutSubTree r 1)

> layoutSubTree :: (Show elem) ⇒ Tree elem → Int → String
> layoutSubTree Empty d = ""

> layoutSubTree (Node Empty v Empty) d =
> 	(replicate d '\t') ++ (show v) ++ "\n"

> layoutSubTree (Node l v r) d =
> 	(layoutSubTree l (d+1))     ++(replicate d '\t')++"  / "  ++ "\n" ++
> 	(replicate d '\t') ++ (show v) ++ "\n" ++
>	(replicate d '\t') ++ "  \\ \n" ++ (layoutSubTree r (d+1)) ++ "\n"
