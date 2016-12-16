import Data.Char (digitToInt)
import Data.List (find)

magicNumber :: Int
magicNumber = isMagic [1..]
    where isMagic (x:xs) = if (digitSum x) == 40
                           then x
                           else isMagic xs

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show