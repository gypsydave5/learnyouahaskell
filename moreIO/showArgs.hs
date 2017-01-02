import System.Environment
import Data.List

main = do
    (x:xs) <- getArgs
    progName <- getProgName
    putStrLn "The first arg is:"
    putStrLn x
    putStrLn "The args are:"
    mapM_ putStrLn xs
    putStrLn "The file name is:"
    putStrLn progName