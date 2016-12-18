data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int
                       } deriving (Eq, Show, Read)

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

beastieBoys = [mca, adRock, mikeD]

mysteryDude = "Person { firstName =\"Michael\"" ++
              ", lastName = \"Diamond\"" ++
              ", age = 43 }"

dude = read mysteryDude :: Person

-- This is my truth...

data MyBool = F | T
    deriving (Show, -- let's things be shown
              Ord,  -- ordering - based on order of value constructor
              Eq)   -- equality, based on type (then vars in constructor)

not' :: MyBool -> MyBool
not' T = F
not' F = T

or' :: MyBool -> MyBool -> MyBool
or' T _ = T
or' F x = x

and' :: MyBool -> MyBool -> MyBool
and' F _ = F
and' T x = x