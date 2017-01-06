-- the pattern for a monad is given by the bind function
-- the bind function applies a function that takes a normal value
-- and returns a lifted (monadic) value, to a lifted (monadic) value,
-- and returns a lifted/fancy/wrapped value-with-a-context.

myBindForMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
myBindForMaybe Nothing f = Nothing
myBindForMaybe (Just x) f = f x

-- here's the Monad typeclass reimplemented for fun

class Applicative m => Monad' m where -- class constriaint - they're all applicative
    return' :: a -> m a
    (>>>=) :: m a -> (a -> m b) -> m b -- bind (normally >>=)

    (>>>) :: m a -> m b -> m b -- (normally >>>)
    x >>> y = x >>>= \_ -> y -- makes sense

    fail' :: String -> m a
    fail' msg = error msg

instance Monad' Maybe where
    return' x = Just x
    Nothing >>>= f = Nothing
    Just x >>>= f = f x
    fail' _ = Nothing