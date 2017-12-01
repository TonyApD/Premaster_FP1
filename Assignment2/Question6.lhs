Marnix Dessing s1014097
Carlo Jessurun s1013793
Tony Lopar s1013792

1a. Function: Int → Int

> g :: Int -> Int
> g n = n + 4711

1b. Function: a → a

> h :: a -> a
> h a = a

1c. Function: (Int, Int) → Int

> addition :: (Int, Int) -> Int
> addition (x, y) = x + y

1d. Function: (a, a) → a

> aa :: (a, a) -> a
> aa (a, _) = a

1e. Function: (a, b) -> a

> ab :: (a, b) -> a
> ab (a, b) = a

2a. Function: (a,a)→(a,a)

> aaaa :: (a, a) -> (a, a)
> aaaa (a, b) = (a, b)

2b. Function: (a,b)→(b,a)

> abba :: (a, b) -> (b, a)
> abba (a, b) = (b, a)

2c. Function: (a→b)→a→b

> abab :: (a -> b) -> a -> b
> abab (a) = a

2d. Function: (a,x)→a

> axa :: (a, x) -> a
> axa (a, x) = a

2e. Function: (x→a→b,a,x)→b - NOT COMPILING

> xabaxb :: (x -> a -> b, a, x) -> b
> xabaxb (f, g, z) =

2f. Function: (a→b,x→a,x)→b

> abxaxb :: (a -> b, x -> a, x) -> b
> abxaxb (f, g, h) = f (g h)

2g. Function: (x→a→b,x→a,x)→b - NOT COMPILING

 xabxaxb :: (x -> a -> b, x -> a, x) -> b
 xabxaxb (f, g, h) = f (g h)

3a. Function: Int → (Int → Int)

> square :: Int -> (Int -> Int)
> square x = \x -> x * x

3b. Function: (Int → Int) → Int

> sum :: (Int -> Int) -> Int
> sum f = f 1

3c. Function: a→(a→a)

> faaa :: a -> (a -> a)
> faaa a = \a -> a

3d. Function: (a→a)→a

> faaa2 :: (a -> a) -> a
> faaa2 f = f f


% > h :: (a, b) -> a
% > h (a, b) -> a

% > i :: (a, a) -> (a, a)
% > i (x, y) = (y, x)
%
% > j :: (a, String) -> (String, String)
% > j (x, y) = (y ++ "a", x)

% > g :: (x -> a -> b, x -> a, x) -> b
% > g (f, g, z) = (f z )(g z)
%
% > k :: (a -> b, x -> a) -> (x -> b)
% > k (f,         g)         a = f(g a)
