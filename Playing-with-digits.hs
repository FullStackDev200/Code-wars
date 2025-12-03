module Codewars.Kata.DigPow where

import Data.Char (digitToInt)

-- >>> digs 12345
-- [1,2,3,4,5]
digs :: Integer -> [Integer]
digs = map (toInteger . digitToInt) . show

-- >>> raiseListToPower [1,2,3,4,5] 2
-- [1,8,81,1024,15625]
raiseListToPower :: [Integer] -> Integer -> [Integer]
raiseListToPower xs p = zipWith (^) xs [p ..]

-- >>> digpow 89 1
-- 1
digpow :: Integer -> Integer -> Integer
digpow x p =
  let ds = digs x
      total = sum (raiseListToPower ds p)
   in if total `mod` x == 0
        then total `div` x
        else -1
