justTimes3 = fmap (*) (Just 3) -- this is Just (3*)

just12 = fmap ($ 4) justTimes3

-- equivalent
just12' = fmap ($ 4) (Just (* 3))

-- but, no way with ordinary average Functors to get from, say Just (*3) and
-- Just 4 to Just 12
-- so...
-- say hello to my little applicative friend
data Maybe' a
  = Nothing'
  | Just' a
  deriving (Show)

class (Functor f) =>
      Applicative' f  where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)

instance Applicative' Maybe' where
  pure' = Just'
  Nothing' <**> _ = Nothing'
  (Just' f) <**> a = fmap f a

(<$$>)
  :: (Functor f)
  => (a -> b) -> f a -> f b
f <$$> x = fmap f x

instance Applicative' [] where
  pure' x = [x]
  fs <**> xs =
    [ f x
    | f <- fs
    , x <- xs ]

instance Applicative' IO where
  pure' = return
  a <**> b = do
    f <- a
    x <- b
    return (f x)

instance Applicative' ((->) r) where
  pure' x = \_ -> x
  f <**> g = \x -> f x (g x)

data ZipList' a =
  ZipList' [a]
  deriving (Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (map f xs)

instance Applicative' ZipList' where
  pure' x = ZipList' (repeat x)
  (ZipList' fs) <**> (ZipList' xs) = ZipList' (zipWith (\f x -> f x) fs xs)

liftA2' :: (Applicative' f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$$> a <**> b

sequenceA' :: (Applicative' f ) => [f a] -> f [a]
sequenceA' [] = pure' []
sequenceA' (x:xs) = (:) <$$> x <**> sequenceA' xs

-- sequenceA' [Just' 1, Just' 2, Just' 3] ==> Just' [1,2,3]

sequenceA'' :: (Applicative' f ) => [f a] -> f [a]
sequenceA'' = foldr (liftA2' (:)) (pure' [])

-- Notes
-- OK so the 'Wrapped Function bit' - the bit at the beginning of the 
-- applied function - applies only to the arguments in the wrapped world - that
-- is to say that for each functor thrown into the mix, the function must have
-- a free variable.
-- Or, in other words, you need the right arity - even in the world you're
-- lifted into, you still need the right arity of arguments for the function
-- you're passing in. How it's applied is up to the definition of <*> for the
-- applicative functor, but it still needs applying to the correct number of
-- functors.
--
-- Further, and scarier, note that the function that is lifted is not
-- necessarily a single function; the list functor could have as many functions
-- of the correct arity in a list - as long as they all return the same type.
-- They wiil be applied to produce combinations _as per the definition of <*>
-- for []

-- Also :t (,,,) WTF??? ... OK, it's the constructor for the tupl data type

gt4lt10Odd = and.sequenceA [(>4),(<10),odd]