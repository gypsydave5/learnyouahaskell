-- type classes describe shared interfaces between types; the interfaces are
-- shared because the types have something in common: equality, ordering, being
-- `mappable` (Functors), etc.
--
-- Consider `*` and `++` - both
-- - take two parameters
-- - return the same type as the two parameters
-- - have a value within the type which works causes the function to work like
-- the identity function
--
-- that is (++[]) is equivalent to id :: [a] -> [a]
-- and (*1) is equivalent to id :: Num a => a -> a
--
-- Also, they exhibit _associativity_ - that is to say
-- (a * b) * c === a * (b * c)
-- (a ++ b) ++ c === a ++ (b ++ c)
--
-- but things like `-` aren't
--
-- And so MONOIDS
--
-- an associative binary function and an identity value wrt that function.
-- You need 1) a function 2) a set of values 3) for that function and the set to
-- exhibit the above. `A` monoid is a set of relations, not a blob.
--
-- 1 is id wrt *; [] is id wrt ++
--
-- Behold

class Monoid' m where
    mempty':: m -- a polymorphic constant - the mempty value for the type. The identity value for a particular monoid
    mappend' :: m -> m -> m
    mconcat' :: [m] -> m
    mconcat' = foldr mappend' mempty' -- default implementation of mconcat based on the other two functions

--
-- Monoid Laws
-- ===========
-- when we make monoid instances, make sure that
-- mappend mempty x = x
-- mappend x mempty = x
-- mappend (mappend x y) z = mappend x (mappend y z) :: associative
--
-- note that mononoids are not necessarily _commutative_
-- i.e. a mappend b /= b mappend a

instance Monoid' [a] where
    mempty' = []
    mappend' = (++)
-- get mconcat' for free - kinda like a method on a superclass (but not)
--
-- So here's a thing - the monoid of (+, Int) is different for the monoid of
-- (*, Int). As the identity values (mempty) are different.
-- 2 + 0 = 2
-- 2 * 1 = 2
--
-- But we obviously need a way to determine which monoid we're using - and yet
-- again it's newtype to the rescue

newtype Product' a = Product' { getProduct' :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product' a) where
    mempty' = Product' 1
    mappend' (Product' x) (Product' y) = Product' (x * y)

newtype Sum' a = Sum' { getSum' :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Sum' a) where
    mempty' = Sum' 0
    mappend' (Sum' x) (Sum' y) = Sum' (x + y)

-- other monoids include the type Bool with the functions || and &&
-- think about it - if you give (|| False) any Bool you'll get that Bool (True
-- or False), and the same goes for (&& True). So the monoids are (False, ||) and
-- (True, &&)

newtype Any' = Any' { getAny' :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' Any' where
    mempty' = Any' False
    mappend' (Any' a) (Any' b) = Any' (a || b)

newtype All' = All' { getAll' :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' All' where
    mempty' = All' True
    mappend' (All' a) (All' b) = All' (a && b)

-- getAll' $ mappend' mempty' mempty' ==> True
-- getSum'.mconcat'.map Sum' $ [1,2,3,4,5] ==> 15
