main = do
    mapM putStrLn ["First line", "Second line"]

-- no idea how these work...
mapM'
    :: (Traversable f, Functor f, Monad m)
    => (a -> m b) -> f a -> m (f b)
mapM' f = sequence . fmap f

mapM_'
    :: (Foldable f, Monad m)
    => (a -> m b) -> f a -> m ()
mapM_' f = foldr ((>>) . f) (return ())