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
import           Control.Monad.State       (MonadState (..), StateT (..))
import           Control.Monad.Except

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
  putStrLn "           THE PROGRAM WE HAVE PARSED           "
  putStrLn "------------------------------------------------"
  case runParser parseProgram contents (0,0) of
    Right (Just ((a,b), _)) -> print a
    Left err -> print err
    Right Nothing -> print "Program Failure"
  return ()

-- runParser :: String -> Program
-- runParser = fst . head . parse parseProgram
-- TODO: WE NEED TO CHANGE THE ERROR MESSAGE HERE
parseProgram :: Parser Char Program
parseProgram
  = bracket (locationReporter (keyword "begin") "Invalid Program start") parseProgram' endingParse
  where
    parseProgram' = do
      traceM "We are parsing the program"
      functions <- many parseFunction
      traceM $ "Functions we have parsed: " ++ show functions
      statement <-  locationReporter parseStatement "Invalid/Missing Program body"
      traceM $ "The statement(s) are: " ++ show statement
      return $ Program functions statement

--    parseProgram' = liftM2 Program (many parseFunction) parseStatement


endingParse :: Parser Char String
endingParse = do
   locationReporter (string "end") "Unexpected Symbol (CHANGE THIS MESSAGE)"
   junk
   unusedInputString <- get
   pos <- getPosition
   traceM $ show (null unusedInputString)
   traceM ("The unused input string is: " ++ unusedInputString)
   if null unusedInputString
     then return "Valid Program"
     else throwError ("Syntax Error: Invalid Program", updateRowPosition pos)

-- specification for the function that we need at the end of parse Program
-- is one that consumes the rest of the string and it fails if it finds
-- something that is not whitespace or is not a comment
