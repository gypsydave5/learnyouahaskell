-- class makes a new type class
class Eq' a where
    (~==) :: a -> a -> Bool
    (~/=) :: a -> a -> Bool
    x ~== y = not (x ~/= y)
    x ~/= y = not (x ~== y)

data TrafficLight = Red | Yellow | Green

-- instance makes a type an instance of a type class
-- Eq' a becomes Eq' Trafficlight - the type is a
instance Eq' TrafficLight where
    Red ~== Red       = True
    Green ~== Green   = True
    Yellow ~== Yellow = True
    _ ~== _           = False

instance Eq TrafficLight where
    Red == Red       = True
    Green == Green   = True
    Yellow == Yellow = True
    _ == _           = False

-- I'm weirdly looking forward to implementing order on numbers...

instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

class (Eq' a) => Ord' a where
    (>>>)  :: a -> a -> Bool
    (<<<)  :: a -> a -> Bool

instance Ord' TrafficLight where
    Red >>> Red       = False
    Red >>> Yellow    = True
    Red >>> Green     = True
    Green >>> Green   = False
    Green >>> Red     = False
    Green >>> Yellow  = False
    Yellow >>> Yellow = False
    Yellow >>> Red    = False
    Yellow >>> Green  = True
    Red <<< Red       = False
    Red <<< Yellow    = False
    Red <<< Green     = False
    Green <<< Green   = False
    Green <<< Red     = True
    Green <<< Yellow  = True
    Yellow <<< Yellow = False
    Yellow <<< Red    = True
    Yellow <<< Green  = False

-- for Eq' to be defined on Maybe m, we need to make sure that it's defined on
-- m too - and to do that we need to throw in a class constraint
instance (Eq' m) => Eq' (Maybe m) where
    (Just x) ~== (Just y) = (x ~== y)
    Nothing ~== Nothing   = True
    _ ~== _               = False
