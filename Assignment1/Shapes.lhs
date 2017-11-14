> {-# LANGUAGE UnicodeSyntax #-}
> module Shapes
> where
> import Unicode

> data Shape
>   =  Circle Double            -- radius
>   |  Square Double            -- length
>   |  Rectangle Double Double  -- length and width
>   deriving (Show)

> showShape ∷ Shape → String
> showShape (Circle r)       =  "circle of radius " ++ show r
> showShape (Square l)       =  "square of length " ++ show l
> showShape (Rectangle l w)  =  "rectangle of length " ++ show l
>                                 ++ " and width " ++ show w

area        ∷ Shape → Double
perimeter   ∷ Shape → Double
center       ∷ Shape → (Double, Double)  -- x- and y-coordinates
boundingBox  ∷ Shape → (Double, Double)  -- width and height
