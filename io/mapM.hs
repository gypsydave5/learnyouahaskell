main = do
    mapM' putStrLn ["First line", "Second line"]

mapM'
    :: (Functor f, Monad m)
    => (a -> b) -> f a -> m (f b)
mapM' f xs = do
    return $ fmap f xs