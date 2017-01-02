bob = head [3, 4, 5, undefined, 2, undefined]

-- is fine - doesn't have to evaluate the undefined values
-- algebraic data type
data CoolBool = CoolBool
  { getCoolBool :: Bool
  } deriving (Show)

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- `helloMe undefined` will make everything blow up, because the constructor for
-- undefined needs to be examined
-- but
newtype CoolerBool = CoolerBool
  { getCoolerBool :: Bool
  } deriving (Show)

helloYou :: CoolerBool -> String
helloYou (CoolerBool _) = "hello"

-- `helloMe undefined` => "hello"
-- Only one constructor, only one possible type, only one possible result - no
-- need to examine the value passed to take apart its constructor.
-- No longer pattern matching, more conversion.
-- Apparently.
--
--
-- Review
-- type : an alias
type IntList = [Int]

-- allows a reference to a type.
oneTwoThree = ([1, 2, 3] :: IntList) ++ ([1, 2, 3] :: [Int]) -- is fine - they're the same types
-- it helps make things clearer - think String and FilePath
--
--
-- newtype: a simple (convenient) wrapper for a type - helps application to
-- typeclass
-- but pretty much a data declaration that has one constructor and one field
--
-- data: everything
-- make