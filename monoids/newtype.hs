-- newtype
-- newtype makes a new type out of an existing type, with the constraint of
-- only being able to have one value constructor with one fielt
data Profession
  = Fighter
  | Archer
  | Accountant
  deriving (Show)

data Race
  = Human
  | Elf
  | Orc
  | Goblin
  deriving (Show)

data PlayerCharacter =
  PlayerCharacter Race
                  Profession
  deriving (Show)

newtype CharList = CharList
  { getCharList :: [Char]
  } deriving (Eq, Show)

-- newtype is useful if you want to 'help' a type to become an instance of
-- a typeclass. i.e. tuples
instance Functor ((,,) a b) where
  fmap f (a, b, c) = (a, b, f c)

-- this works fine, but only if we want to apply our function to... hold on, why
-- can't we apply it to multiple fields... anyway.
-- this works fine if we want to apply the function to the last field, but to
-- get to the first field we need to do a little bit of work rearranging the
-- type parameters.
newtype Triple c b a = Triple
  { getTriple :: (a, b, c)
  } deriving (Eq, Show)

instance Functor (Triple c b) where
  fmap f (Triple (a, b, c)) = Triple (f a, b, c)
  -- so now fmap (+2) (2,2,2) => (2,2,4)
-- whereas fmap (+2) (Triple (2,2,2)) => Triple {getTriple = (4,2,2)