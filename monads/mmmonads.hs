class Functor m =>
      Monad' m  where
  return' :: a -> m a
  (>>>=) :: m a -> (a -> m b) -> m b
  (>>>) :: m a -> m b -> m b
  x >>> y = x >>>= const y
  fail' :: String -> m a
  fail' = error

instance Monad' Maybe where
  return' = Just
  Nothing >>>= f = Nothing
  Just x >>>= f = f x
  fail' _ = Nothing

-- some shit with birds
type Birds = Int

type Pole = (Birds, Birds)

landLeft' :: Birds -> Pole -> Pole
landLeft' n (left, right) = (left + n, right)

landRight' :: Birds -> Pole -> Pole
landRight' n (left, right) = (left, right + n)

x -: f = f x

-- but this only fails after all the calculations have run - what if the bird
-- guy gets knocked off
-- before then?
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- so
landing = landRight 1 (0,0) >>>= landLeft 2 >>>= landRight 3 -- etc
--
-- or better
fallOver = return (0,0) >>>= landLeft 1 >>>= landRight 4 >>>= landLeft (-1) >>>= landRight (-2)