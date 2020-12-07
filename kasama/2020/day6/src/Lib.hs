module Lib (someFunc) where

import Data.List.Split
import Data.Set (Set, fromList, intersection)
import qualified Input

-- >>> numUniqueAnswers "abracadabra"
-- 5
numUniqueAnswers = length . uniqueAnswers

-- >>> map (uniqueAnswers) $ take 2 groups
-- [fromList "bfilorst",fromList "aegjlnoqrsxyz"]
uniqueAnswers = fromList . filter (/= '\n')

-- >>> map personalAnswers $ take 1 groups
-- [[fromList "bfilorst",fromList "bfilorst",fromList "bfilorst",fromList "bfilorst",fromList "bfilorst"]]
personalAnswers :: [Char] -> [Set Char]
personalAnswers group = map fromList $ splitOn "\n" group

-- >>> map (checkAllAnswers) $ take 2 groups
-- [fromList "bfilorst",fromList "aegjlnqrxyz"]
checkAllAnswers group = foldl1 intersection (personalAnswers group)

-- >>> take 2 groups
-- ["ltbriofs\nbitolfsr\nolitfrbs\nsbirloft\nsbrftiol","erlnjxsqaygzo\neznagxlqjry\nznelyrjaqgx\nynxelzgrjaq"]
groups :: [String]
groups = splitOn "\n\n" Input.input

someFunc :: IO ()
someFunc = do
  putStr "Sum of answers someone said yes per group: "
  print $ sum $ map numUniqueAnswers groups
  putStr "Sum of answers everyone said yes per group: "
  print $ sum $ map (length . checkAllAnswers) groups
