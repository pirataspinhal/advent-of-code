module Main where

import Control.Monad (void)
import Data.List (group, sort)
import qualified Data.Map as Map
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)

main = do
  fish <- parsedInput school
  putStr "Part One: "
  -- print . length $ simulate 6 2 80 fish
  -- putStr "Part Two: "
  -- print . length $ simulate 6 2 256 fish

parsedInput :: Parser b -> IO b
parsedInput p = do
  Right parsed <- parse' p <$> input
  return parsed

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

eor :: Parser ()
eor = choice [void $ try $ char '\n', void $ try $ char ',', void eof]

number :: Parser Int
number = do
  num <- spaces *> many1 digit
  return $ read num

school :: Parser School
school = do
  fishes <- many $ number <* eor
  return $ foldl (\school f -> Map.insert (head f) (length f) school) Map.empty (group $ sort fishes)

input = readFile "inputs/day6.in"

type Fish = Int

type School = Map.Map Fish Int

simulate :: Int -> Int -> Int -> [Fish] -> [Fish]
simulate lifespan birthPenalty days school = foldl (\school _ -> simulate' school) school [1 .. days]
  where
    simulate' [] = []
    simulate' (0 : school) = lifespan : (lifespan + birthPenalty) : simulate' school
    simulate' (fish : school) = (fish - 1) : simulate' school

-- ssimulate :: Int -> Int -> Int -> School -> School
-- ssimulate lifespan birthPenalty days school = foldl (\school _ -> simulate' school) school [1 .. days]
--   where
--     simulate' school = foldl (\school life -> ) school lifes
--     lifes = [0..lifespan+birthPenalty]
