module Vector (Vector) where

data Vector a = Vector a a a
    deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector x y z) m = (Vector (x*m) (y*m) (z*m))

dotprod :: (Num a) => Vector a -> Vector a -> a
dotprod (Vector x1 y1 z1) (Vector x2 y2 z2) = (x1*x2) + (y1*y2) + (z1*z2)