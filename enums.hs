data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (
        Eq,      -- a == a (based on value constructor)
        Ord,     -- a < a (based on order of value constructors)
        Show,    -- can be made into a string (show)
        Read,    -- can be constructed from a string (read)
        Bounded, -- has a greatest and a least value. (minBound, maxBound)
        Enum     -- total order. (succ, pred)
    )

type Days = [Day]

week :: Days
week = [Monday ..]
