import Data.List (tails, isPrefixOf)

tails' :: [a] -> [[a]]
tails' [] = []
tails' xs@(_:ys) = xs : tails' ys

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] []         = True
isPrefixOf' [] _          = True
isPrefixOf' _ []          = False
isPrefixOf' (x:xs) (y:ys) = if x == y
                            then isPrefixOf' xs ys
                            else False

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x acc -> (f x) || acc) False

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn _ []  = False
isIn ns hs = any' (isPrefixOf' ns) $ tails' hs

