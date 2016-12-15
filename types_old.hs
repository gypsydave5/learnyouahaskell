-- Int a
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer (really big ... practically unbounded)
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Float (single precision)
circumference :: Float -> Float
circumference r = 2 * pi * r
