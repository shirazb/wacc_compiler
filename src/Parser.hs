module Parser (parseProgram) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Debug.Trace

{- Local Imports -}

import Parsers.Expression
import Parsers.Function
import Parsers.Statement
import Parsers.Type
import Utility.Definitions
import Utility.BasicCombinators
import Utility.Declarations

parseProgram :: Parser Program
parseProgram
  = bracket (string "begin") parseProgram' (string "end")
  where
    parseProgram' = liftM2 Program (many parseFunc) parseStatement
