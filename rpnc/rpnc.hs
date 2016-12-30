solve :: String -> Double
solve expression = head (foldl foldingFunction [] (words expression))
  where
    foldingFunction (x:y:xs) "*" = (x * y) : xs
    foldingFunction (x:y:xs) "+" = (x + y) : xs
    foldingFunction (x:y:xs) "-" = (x - y) : xs
    foldingFunction (x:y:xs) "/" = (x / y) : xs
    foldingFunction (x:y:xs) "^" = (x ** y) : xs
    foldingFunction (x:xs) "l" = log x : xs
    foldingFunction xs "s" = [sum xs]
    foldingFunction xs "p" = [product xs]
    foldingFunction xs num = read num : xs