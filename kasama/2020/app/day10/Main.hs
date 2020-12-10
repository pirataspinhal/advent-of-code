module Main where

import Data.Sort ( sort )
import Graph
import Data.Graph

main :: IO ()
main = do
  input <- input
  putStr "number of 1-jolts multiplied by the number of 3-jolts: "
  print $ multOneThree $ findJoltDifferences input

input :: IO [Int]
input = fmap read . lines <$> readFile "inputs/day10.in"

testInput = [ 28
            , 33
            , 18
            , 42
            , 31
            , 14
            , 46
            , 20
            , 48
            , 47
            , 24
            , 23
            , 49
            , 45
            , 19
            , 38
            , 39
            , 11
            , 1
            , 32
            , 25
            , 35
            , 8
            , 17
            , 7
            , 9
            , 4
            , 2
            , 34
            , 10
            , 3
            ]

multOneThree (a, _, b) = a * b

-- >>> findJoltDifferences smallExample
-- (7,0,5)
findJoltDifferences transformers = calcJolts initialJolts sorted
  where
    initialJolts
      | value == 1 = (1, 0, 0)
      | value == 2 = (0, 1, 0)
      | value == 3 = (0, 0, 1)
      where
        value = head sorted
    sorted = sort transformers
    calcJolts jolts [] = jolts
    calcJolts (ones, twos, threes) [_] = calcJolts (ones, twos, threes + 1) []
    calcJolts (ones, twos, threes) (a:b:others)
      | difference == 1 = calcJolts (ones + 1, twos, threes) next
      | difference == 2 = calcJolts (ones, twos + 1, threes) next
      | difference == 3 = calcJolts (ones, twos, threes + 1) next
      | otherwise = calcJolts (ones, twos, threes) next
      where
        next = b:others
        difference = abs $ a - b

makeGraph connections = toCGraph $ graphFromEdges edges
  where
    edges = map (\(a, b) -> (a, a, [b])) connections

-- >>> connectedTransformers [1,3,8,2,4,7]
-- [(1,3),(1,2),(1,4),(3,4),(2,3),(2,4),(4,7),(7,8)]
connectedTransformers :: [Int] -> [(Int, Int)]
connectedTransformers transformers = transformerCombinations
  where
    transformerCombinations = [ (snd a, snd b)
                              | a <- indexedTransformers
                              , b <- indexedTransformers
                              , fst a /= fst b
                              , abs (snd a - snd b) <= 3
                              , snd a < snd b
                              ]
    indexedTransformers = zip [1..] transformers
