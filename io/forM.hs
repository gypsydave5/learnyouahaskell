import Control.Monad (forM)

main' = do
    colors <-
        forM
            [1, 2, 3, 4]
            (\a -> do
                 putStrLn $
                     "Which color do you associate with the number " ++
                     show a ++ "?"
                 color <- getLine
                 return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

-- "the (\a -> do ... ) lambda" is an abomination that we should ignore and
-- rewrite...
-- better as ... ?
main = do
    colors <- getColors [1, 2, 3, 4]
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

getColors
    :: Show a
    => [a] -> IO ([String])
getColors = mapM getColor
  where
    getColor
        :: Show a
        => a -> IO (String)
    getColor a = do
        putStrLn $
            "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color