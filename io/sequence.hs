main = do
    rs <- sequence [getLine, getLine, getLine]
    putStrLn $ unwords rs