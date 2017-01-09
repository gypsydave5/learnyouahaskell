import MyMonad

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
landing = landRight 1 (0, 0) >>>= landLeft 2 >>>= landRight 3 -- etc

--
-- or better
fallOver =
  return (0, 0) >>>= landLeft 1 >>>= landRight 4 >>>= landLeft (-1) >>>=
  landRight (-2)

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- yeah whatever -- just use >>> to break the chain
slipUp = return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 2

-- anyway, do notation
foo :: Maybe String
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

foodo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

-- so now we can do the tightrope schtick with
routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 2 second

shitRoutine :: Maybe Pole
shitRoutine =
  case Just (0, 0) of
    Nothing -> Nothing
    Just start ->
      case landLeft 2 start of
        Nothing -> Nothing
        Just first ->
          case landRight 2 first of
            Nothing -> Nothing
            Just second -> landLeft 1 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

-- if the pattern match fails, to notation uses the fail function,
-- which is implemented as Nothing for just
nowt :: Maybe Char
nowt = do
  (x:xs) <- Just ""
  return x

-- List monads
instance Monad' [] where
  return' x = [x]
  xs >>>= f = concat (map f xs)
  fail' _ = []