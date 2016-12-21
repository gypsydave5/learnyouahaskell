import Control.Monad (when)

main = do
    input <- getLine
    when' (input == "SWORDFISH") $ putStrLn input

-- equivalent to
main' = do
    input <- getLine
    if (input == "SWORDFISH")
        then putStrLn input
        else return ()

when' :: Bool -> IO () -> IO ()
when' True i = i
when' False _ = return ()