-- kinds are the type of a type
{-
 This is how the multiline comments look in Haskell. Love it.

 OK so, the kind of a type is it's type, and it describes the types it takes to build it

 :kind Int
 Int :: *

 * means 'type' - it's a concrete type

 :kind Maybe
 Maybe :: * -> *

 Maybe takes a concrete type and returns a new concrete type
 :kind Maybe Int
 Maybe Int :: *

 kinds are (kinda) like functions

 :type not
 not :: Bool -> Bool
 :type not True
 not True :: Bool
 (the arguments become specific and get pushed to the left)

-}
thingy
    :: (Either String Int) -> Int
thingy x = 5