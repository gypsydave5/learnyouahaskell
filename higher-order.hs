-- Look - curry
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z
-- delicious curry...

-- higher and higher
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Note that `->` is right associative - i.e `a -> a -> a` === `a -> (a -> a)`
-- (which is why curry is so delicious)

-- Look - recursion with higher order functions
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- note that map and filter could be implemented with a predicate

largestDivisible :: Integer -> Integer -> Integer
largestDivisible x y = head (filter' p [x, (x - 1)..])
    where p x = mod x y == 0

-- sum (takeWhile (<1000) (filter odd (map (^2) [1..])))
-- sum (takeeWhile (<1000) [m | m <- [ n^2 | n <- [1..] ], odd m])

-- Collatz Chain
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (div n 2)
    | odd n = n : chain (n*3 + 1)

numLongChains :: Integer
numLongChains = fromIntegral (length (filter isLong (map chain [1..100])))
    where isLong xs = length xs > 15

-- Lllllllllllllllllambdas!

numLongChains' :: Integer
numLongChains' = fromIntegral(length (filter (\xs -> length xs > 15) (map chain [1..100])))

-- equivalent
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

-- another way to flip
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- Foldl
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- this seems silly to me... how about we use that delicious CURRY
-- Schoenfinkelization!
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- Foldr
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []
-- which is to also say...
map''' :: (a -> b) -> [a] -> [b]
map''' f = foldl (\acc x -> acc ++ [f x]) []
-- but ++ is much less efficient than cons, so foldr best for building lists

elem' :: (Eq a) => a -> [a] -> Bool
elem' a = foldr (\x acc -> x == a || acc) False

bookElem :: (Eq a) => a -> [a] -> Bool
bookElem y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- Always worth doing it yourself to see how your own instincts are.

-- foldl1 foldr1 -- make the first element the accumulator
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- OK - now do ... reverse!
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- Now... product!
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- Next... filter!
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

-- last... last!
last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

-- To infinity
and' :: [Bool] -> Bool
and' = foldr (&&) True

