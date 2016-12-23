main = interact respondPalindrome

respondPalindrome :: String -> String
respondPalindrome = unlines . map response . lines
  where
    response xs =
        if isPalindrome xs
            then "palindrome"
            else "not palindrome"

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x