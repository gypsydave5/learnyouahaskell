-- Define Logic (an aside in pattern matching)
(&&~) :: Bool -> Bool -> Bool
True  &&~ x = x
False &&~ _ = False

(||~) :: Bool -> Bool -> Bool
True  ||~ _ = True
False ||~ x = x

not' :: Bool -> Bool
not' True  = False
not' False = True

