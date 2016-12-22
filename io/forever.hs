import Control.Monad (forever)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)

main =
    forever' $
    do l <- prompt "Give me some input: "
       putStrLn $ map toUpper l

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout -- need to flush stdout to get the prompt to show...
    getLine

forever' :: IO a -> IO a
forever' x = do
    x
    forever' x