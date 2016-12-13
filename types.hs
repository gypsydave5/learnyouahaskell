-- [Char] is a string - funny that
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase string = [ c | c <- string, elem c ['A'..'Z'] ]

-- Int is an integer  the size of a system word (i.e. 64 bit)
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer is an (almost) unbounded integer
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Float is a single precision real floating point number
circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double has double the precision of Float
circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Tuples have the type derived from their constitutent types
sumTriple :: (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

-- -- Type Variable
-- Are like generics. But better.
-- -- Type Classes
-- Are not the same as OO classes. But they are a bit like them.
--