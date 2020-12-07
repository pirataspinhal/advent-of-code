module Lib
    ( someFunc
    ) where

import Input

mapWidth = length $ head Input.input

treeChar = '#'

findTrees' :: (Int, Int) -> Int -> [[Char]] -> (Int, Int) -> Int -> Int
findTrees' _ _ [] _ trees = trees
findTrees' moves iter (line:lines) (offsetW, offsetH) trees = findTrees' moves nextIter lines (nextOffsetW, nextOffsetH) newTreeSum
  where nextIter = iter + 1
        nextOffsetW = if isValidIter then (offsetW + moveLeft) `mod` mapWidth else offsetW
        nextOffsetH = if isValidIter then offsetH + moveDown else offsetH
        (moveLeft, moveDown) = moves
        isValidIter = iter == offsetH
        newTreeSum = if (line !! offsetW) == treeChar && isValidIter then trees + 1 else trees

findTrees :: (Int, Int) -> [[Char]] -> Int
findTrees moves lines = findTrees' moves 0 lines (0, 0) 0

someFunc :: IO ()
someFunc = do
  putStrLn $ "testing for " ++ show testInput ++ ":"
  print res
  putStr "Multiplying: "
  print $ product res
  where res = map (`findTrees` Input.input) testInput
        testInput = [(1,1), (3,1), (5,1), (7,1), (1,2)]
