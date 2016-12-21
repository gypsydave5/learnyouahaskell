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

insert :: (Eq k) => k -> v -> (AssocList k v) -> (AssocList k v)
insert k v EmptyAssocList           = AssocListItem k v EmptyAssocList
insert k v (AssocListItem k' v' ls) = if k' == k
                                      then AssocListItem k v ls
                                      else AssocListItem k' v' $ insert k v ls

remove :: (Eq k) => k -> AssocList k v -> (AssocList k v)
remove k EmptyAssocList           = EmptyAssocList
remove k (AssocListItem k' v' ls) = if k' == k
                                    then ls
                                    else AssocListItem k' v' $ remove  k ls

get :: (Eq k) => k -> AssocList k v -> Maybe v
get k EmptyAssocList          = Nothing
get k (AssocListItem k' v ls) = if k' == k
                                then Just v
                                else get k ls

-- data is applied functions; functions are uninitialized data...
-- there is no data; there are no functions; they are all types and the same

instance Functor (AssocList k) where
    fmap f EmptyAssocList = EmptyAssocList
    fmap f (AssocListItem k v ls) = AssocListItem k (f v) $ fmap f ls


