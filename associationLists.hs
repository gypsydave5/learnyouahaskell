import qualified Data.Map as Map
import Data.Char (isDigit, digitToInt)

-- Implementing your own associative list

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key = foldr getKey Nothing
    where getKey (k, v) acc = if key == k
                              then Just v
                              else acc

-- using Data.Map.Map
phoneBook :: Map.Map String String
phoneBook = Map.fromList
    [
        ("betty", "555-2938"),
        ("bonnie", "452-2928"),
        ("patsy", "493-2928"),
        ("lucille", "205-2928"),
        ("wendy", "939-8282"),
        ("penny", "853-2492")
    ]

phoneBookList =
    [("betty", "555-2938")
    ,("betty", "342-2492")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("patsy", "827-9162")]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
    where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
