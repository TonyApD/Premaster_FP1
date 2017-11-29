> g :: Int -> Int
> g n = n + 4711

> h :: (a, b) -> a
> h (a, b) -> a

> i :: (a, a) -> (a, a)
> i (x, y) -> (y, x)

> j :: (a, String) -> (String, String)
> j (x, y) -> (y ++ "a", x)

> g :: (x -> a -> b, x -> a, x) -> b
> g (f, g, z) = (f z )(g z)

> k :: (a -> b, x -> a) -> (x -> b)
> k (f,         g)         a = f(g a)
