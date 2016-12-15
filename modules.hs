import Data.List (nub, words, group, sort)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

sentenceLength :: String -> Int
sentenceLength = length.words

wordCounts :: String -> [(String, Int)]
wordCounts s = map (\ws -> (head ws, length ws)) (group.sort.words s)

