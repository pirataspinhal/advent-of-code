module Lib.Parser where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

eor :: Parser ()
eor = choice [ void $ try $ char '\n', void eof ]

number :: Parser Int
number = do
  num <- many digit
  return $ read num

magic = putStrLn "magic"
