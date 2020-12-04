module Lib
    ( someFunc
    ) where

import Expenses (expenses)
import Data.Maybe

someFunc :: IO ()
someFunc = do
  putStrLn "First exercise:"
  print targeted
  putStrLn "Second exercise:"
  print trioTargeted

target = 2020


-- Exercise one
sumPair (a, b) = if a + b == target then Just (a * b) else Nothing

pairs = [(a, b) | a <- expenses, b <- expenses]
values = map sumPair pairs
targeted = filter isJust values

-- Exercise two
sumTrio (a, b, c) = if a + b + c == target then Just (a * b * c) else Nothing

trios = [(a, b, c) | a <- expenses, b <- expenses, c <- expenses]
trioValues = map sumTrio trios
trioTargeted = filter isJust trioValues
