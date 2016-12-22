main = do
    rs <- sequence [getLine, getLine, getLine]
    sequence $ map putStr rs