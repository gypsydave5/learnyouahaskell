-- making our own list

data List' a = Empty' | Cons' a (List' a)
    deriving (Show, Read, Eq, Ord)

-- recursively defined data structure - it's lisp

-- operator definition with fixity
infixr 5 :-:
data List a = Empty | a :-: (List a)
    deriving (Show, Read, Eq, Ord)
-- same as the above

infixr 5 -++
(-++) :: List a -> List a -> List a
Empty  -++ ys = ys
(x:-:xs) -++ ys = x :-: (xs -++ ys)

-- wait there ... destructuring isn't magic ... it's just pulling things
-- apart by their function signature ...
--
-- Unwrapping a value is just a constructor backwards
(Left variable) = Left "value"

-- binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
