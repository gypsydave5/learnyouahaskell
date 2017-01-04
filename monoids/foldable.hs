import Data.Monoid

data Tree a
  = EmptyTree
  | Node a
         (Tree a)
         (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = mconcat [foldMap f l, f x, foldMap f r]

testTree =
  Node
    5
    (Node 3 (Node 1 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree))
    (Node 9 (Node 8 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree))

-- enjoy all these lovely foldy things
allTheNodes = foldMap (: []) testTree

anyGtFifteen = getAny $ foldMap (Any . (> 15)) testTree

anyThree = getAny $ foldMap (Any . (== 3)) testTree

addEmUp = getSum $ foldMap Sum testTree

timesEmAll = getProduct $ foldMap Product testTree

length' t = appEndo (foldMap (\_ -> Endo (+ 1)) t) 0
-- don't worry abount this bit of madness...