main = interact' shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((> 10) . length) . lines

interact' :: (String -> String) -> IO ()
interact' f = do
    contents <- getContents
    putStr (shortLinesOnly contents)