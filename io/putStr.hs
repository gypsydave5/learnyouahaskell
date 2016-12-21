main = do
    putStr' "Hello you\n"
    putStrLn' "Hello again"

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' (x:xs) = do
    putChar x
    putStrLn xs