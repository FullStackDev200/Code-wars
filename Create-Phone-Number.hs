module CreatePhoneNumber where

import Data.Char

-- >>> wrapInParentheses "skibidi"
-- "(skibidi)"
wrapInParentheses :: String -> String
wrapInParentheses "" = ""
wrapInParentheses x = "(" ++ x ++ ")"

-- >>> createPhoneNumber [1,2,3,4,5,6,7,8,9]
-- "(123) 456-789"
createPhoneNumber :: [Int] -> String
createPhoneNumber x =
  wrapInParentheses (map intToDigit (take 3 x))
    ++ " "
    ++ map intToDigit (take 3 (drop 3 x))
    ++ "-"
    ++ map intToDigit (drop 6 x)
