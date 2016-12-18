import qualified Data.Map as Map

-- type aliases
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = [
        ("betty", "555-2938"),
        ("bonnie", "452-2938"),
        ("patsy", "493-2938"),
        ("lucille", "205-2938"),
        ("wendy", "939-2938"),
        ("penny", "853-2938")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = elem (name, pnumber) pbook

-- parameterized type values

type AssocList key value = [(key, value)]

get :: (Eq key) => key -> AssocList key value -> Maybe value
-- ^^^ this is the only bit the type bit matters in ^^^
get x = foldl (\acc (k, v)  -> if x == k then Just v else acc) Nothing

-- partially applied type values
type IntMap = Map.Map Int