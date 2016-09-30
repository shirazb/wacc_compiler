{-
A parser built using parser combinators for the WACC language. Parser
combinators are used because of the flexibility and modularity that they
offer. Building a parser combinator in Haskell also serves as a learning
experience to learn the more advanced features of Haskell. The parser
currently has no error handling.
-}

module Parser (runParser) where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           System.Environment

{- Local Imports -}

import           Parsers.Expression
import           Parsers.Function
import           Parsers.Lexer
import           Parsers.Statement
import           Utility.BasicCombinators
import           Utility.Declarations
import           Utility.Definitions

-- runParser :: Parser t a -> [t] -> Position -> Either Err (Maybe((a,[t]), Position))

main = do
  args <- getArgs
  let filename = head args
  contents <- readFile filename
  putStrLn "------------------------------------------------"
  putStrLn "THE PROGRAM WE HAVE PARSED"
  putStrLn "------------------------------------------------"
  case runParser parseProgram contents (0,0) of
    Right (Just ((a,b), _)) -> print a
    _ -> print "[]"
  return ()

-- runParser :: String -> Program
-- runParser = fst . head . parse parseProgram

parseProgram :: Parser Char Program
parseProgram
  = bracket (keyword "begin") parseProgram' (string "end")
  where
    parseProgram' = liftM2 Program (many parseFunction) parseStatement



-- keyword :: String -> Parser String
-- keyword k = do
--   kword <- leadingWS (string k)
--   check isSpace <|> check isPunctuation <|> check isComment
--   junk
--   return kword
--
-- endingParse = do
--    string "end"
--    junk s
--    st <- get
--    if length (st) != 0 then mzero else result "!"



-- Y: end
-- Y: end___
-- N: endasuidhas
-- N: end    sjdhasd
-- Y: end   #asjkdhaskjda
-- Y: end#dhasiudhasd


-- specification for the function that we need at the end of parse Program
-- is one that consumes the rest of the string and it fails if it finds
-- something that is not whitespace or is not a comment

-- sdgjgs \nsdjqws
