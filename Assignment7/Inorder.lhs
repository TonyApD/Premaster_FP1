> data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)


> boom = Node (Node Empty "node 1" Empty) "root" (Node Empty "node 2" Empty)
> boom2 = Node (Node Empty "node 1,2" Empty) "root,2" (Node Empty "node 2,2" Empty)

> inorder :: Tree elem -> [elem]
> inorder Empty = [ ]
> inorder (Node l a r) = inorder l ++ [a] ++ inorder r

> inorderCat :: Tree elem -> [elem] -> [elem]
> inorderCat Empty xs = xs
> inorderCat (Node l a r) xs = (inorderCat l (a:inorderCat r xs))

-- The new implementation isn't more efficient in terms of running time,
-- when the functions below are used to generate test trees.
> skewed :: Int -> Tree Int
> skewed 0 = Empty
> skewed h = Node (skewed (h-1)) h Empty

> fullTree :: Int -> Tree Int
> fullTree 0 = Empty
> fullTree h = Node (fullTree (h-1)) h (fullTree (h-1))

-- Repeat the exercise for preorder and postorder:
> preorderCat :: Tree elem -> [elem] -> [elem]
> preorderCat Empty xs = xs
> preorderCat (Node l a r) xs = a:(preorderCat l (preorderCat r xs))

> postorderCat :: Tree elem -> [elem] -> [elem]
> postorderCat Empty xs = xs
> postorderCat (Node l a r) xs = postorderCat l (postorderCat r (xs:a:[])) ++ [a] --How?! Now the running time is quadratic again.





