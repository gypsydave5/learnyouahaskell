import Tree

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

-- "Types that can act like a box can be functors"
-- and you can make boxes out of trees...

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
-- so we define what fmap is for Tree, and so now it is a Functor too :D
-- Behold my functorialness...

data Either' a b = Left' a | Right' b
    deriving (Show, Eq, Ord, Read)

instance Functor (Either' a) where
    fmap f (Right' x) = Right' (f x)
    fmap f (Left' x)  = Left' x

-- only map over the 'win' data type

data AssocList k v = EmptyAssocList | AssocListItem k v (AssocList k v)
    deriving (Show, Eq, Ord, Read)

insert :: (Eq k) => (AssocList k v) -> k -> v -> (AssocList k v)
insert x k v = AssocListItem k v x
