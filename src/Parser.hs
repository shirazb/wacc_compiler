module Parser (parseProgram) where

import Control.Applicative
import Control.Monad
import Debug.Trace

{- Local Imports -}

import Parsers.Function
import Parsers.Statement
import Utility.Definitions
import Utility.BasicCombinators
import Utility.Declarations

parseProgram :: Parser Program
parseProgram
  = bracket (string "begin") parseProgram' (string "end")
  where
    parseProgram' = liftM2 Program (many parseFunction) parseStatement
