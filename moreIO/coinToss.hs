import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = (firstCoin, secondCoin, thirdCoin)
  where
    (firstCoin,gen') = random gen
    (secondCoin,gen'') = random gen'
    (thirdCoin,_) = random gen''

randoms'
    :: (RandomGen g, Random a)
    => g -> [a]
randoms' gen = value : randoms' gen'
  where
    (value,gen') = random gen