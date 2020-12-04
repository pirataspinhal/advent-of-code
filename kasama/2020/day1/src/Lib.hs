module Lib
    ( someFunc
    ) where

import Expenses (expenses)

someFunc :: IO ()
someFunc = do
  putStrLn "First exercise:"
  print $ head targeted
  putStrLn "Second exercise:"
  print $ head trioTargeted

target = 2020

-- Exercise one
targeted = [ a * b | a <- expenses, b <- expenses, a + b == target ]

-- Exercise two
trioTargeted = [a * b * c | a <- expenses, b <- expenses, c <- expenses, a + b + c == target]
