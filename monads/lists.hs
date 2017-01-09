import MyMonad

instance Monad' [] where
    return' x = [x]
    xs >>>= f = concat (map f xs)
    fail' _ = []

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)

-- same as
listOfTuplesComp :: [(Int, Char)]
listOfTuplesComp = [(n, ch) | n <- [1,2], ch <- ['a', 'b']]

-- same as
listOfTuplesM :: [(Int, Char)]
listOfTuplesM = [1,2] >>>=
        (\n -> ['a', 'b'] >>>=
        (\ch -> return (n, ch)))