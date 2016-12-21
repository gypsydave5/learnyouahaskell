main = do
    line <- getLine
    if null line
        then return () -- `return` makes an I/O action out of a value - () in this case
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words -- this is now my all time favourite 'look it's right associative!' example