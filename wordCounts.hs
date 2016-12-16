import Data.List (nub, group, sort)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

sentenceLength :: String -> Int
sentenceLength = length.words

wordCounts :: String -> [(String, Int)]
wordCounts = map (\ws -> (head' ws, length' ws)) . group' . sort' . words'

-- implementing all the above ...

-- takeWhile and dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) = if p x
                      then dropWhile' p xs
                      else xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x
                      then x : takeWhile' p xs
                      else []

isSpace :: Char -> Bool
isSpace = (==' ')

-- break and words
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p xs = (takeWhile (not.p) xs, dropWhile (not.p) xs)

words' :: String -> [String]
words' s = case dropWhile' isSpace s of -- keep dropping spaces
    [] -> [] -- if what you've got left is empty, return empty (base case)
    s' -> w : words' s'' -- otherwise take the first word from break
        where (w, s'') = break' isSpace s' -- join it to recursion on rest of sentence

-- insert and sort
insert' :: (Ord a) => a -> [a] -> [a]
insert' x [] = [x]
insert' x ys@(y:ys')
    | x > y     = y : insert' x ys'
    | otherwise = x : ys

sort' :: (Ord a) => [a] -> [a]
sort' = foldr insert' []

-- span and group
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ []     = ([], [])
span' p xs@(x:xs')
    | p x       = (x : ys, zs)
    | otherwise = ([], xs)
    where (ys, zs) = span p xs'

-- equivalent
span'' :: (a -> Bool) -> [a] -> ([a], [a])
span'' _ []     = ([], [])
span'' p xs@(x:xs')
    | p x       = let (ys, zs) = span'' p xs'
                  in (x : ys, zs)
    | otherwise = ([], xs)

group' :: Eq a => [a] -> [[a]]
group' []       = []
group' xs@(x:_) = let (ys, zs) = span' (==x) xs
                  in ys : group' zs

group'' :: Eq a => [a] -> [[a]]
group'' []       = []
group'' xs@(x:_) = sameXs : group' restXs
    where (sameXs, restXs) = span' (==x) xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

head' :: [a] -> a
head' []    = error "empty list"
head' (x:_) = x

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

