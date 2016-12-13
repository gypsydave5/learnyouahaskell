lucky :: Int -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- shiiiit - look, type classes - we don't care what 'a' is
first :: (a, b, c) -> a
first (x, _, _) = x

-- constrainey
first' :: Num a => (a, b, c) -> a
first' (x, _, _) = x

head' :: [a] -> a
head' [] = error "No head of empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "There is no tail to the empty list"
tail' (_:xs) = xs

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is '" ++ [x] ++ "'"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / height^2 <= 18.5 = "You're underweight, eat more!"
    | weight / height^2 <= 25.0 = "Looking good!"
    | weight / height^2 <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor"

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= fat    = "You're overweight. Let's work out together!"
    | otherwise     = "You're obese. Go see a doctor"
    where bmi = weight / height^2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

fizzbuzz :: (Integral a, Show a) => a -> String
fizzbuzz x
    | mod x 15 == 0 = "FizzBuzz"
    | mod x 5 == 0 = "Buzz"
    | mod x 3 == 0 = "Fizz"
    | otherwise = show x