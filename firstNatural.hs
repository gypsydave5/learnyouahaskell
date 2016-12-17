import Data.Char (digitToInt)
import Data.List (find)

magicNumber :: Int
magicNumber = isMagic [1..]
    where isMagic (x:xs) = if (digitSum x) == 40
                           then x
                           else isMagic xs

firstTo :: Int -> Maybe Int
firstTo n = find'' ((==n).digitSum) [1..]

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ []     = Nothing
find' p (x:xs) = if p x
                 then Just x
                 else find' p xs

find'' :: (a -> Bool) -> [a] -> Maybe a
find'' p = foldr (\x acc -> if p x then Just x else acc) Nothing