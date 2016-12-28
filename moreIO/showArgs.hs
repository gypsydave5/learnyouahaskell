import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The args are:"
    mapM_ putStrLn args
    putStrLn "The file name is:"
    putStrLn progName