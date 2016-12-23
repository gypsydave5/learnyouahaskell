import Control.Monad (forever)
import Data.Char (toUpper)

main =
    forever' $
    do l <- getLine
       putStrLn $ map toUpper l

-- let's write forever again... for fun
forever'
    :: IO (a) -> IO (a)
forever' i = do
    i
    forever i