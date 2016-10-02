{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-
This module defines the parser type which will be used in the rest of the program. It also defines a number of typeclass instances,
enabling us to benefit from the functions that these typeclasses provide. The functions provided by the typeclasses are used in defining
parser combinators. Further documentation regarding these typeclasses and their uses can be found on the Haskell wiki.
-}
module Utility.Declarations where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State       (MonadState (..), StateT (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.List                 (nub)
import           Control.Monad.Except

type Position
  = (Int, Int)

type Err
  = (String, Position)

-- [t] refers to a list of tokens
-- Either Err a
-- Either Err Int
-- Just 5 or Nothing
-- MaybeT (Either Err) a
-- runMaybeT
-- Either Err (Maybe Int)

--          inner monad  type
-- (MaybeT (Either Err) a)
-- m (Maybe a)
-- Either Err (Maybe Int)

-- StateT Position (MaybeT (Either Err)) a
-- runStateT
-- Either Err (Maybe ((Int,Position))
-- StateT [t] (StateT Position (MaybeT (Either Err))) a
-- Either Err (Maybe ((Int,[t]), Position))
newtype Parser t a = Parser {parse :: StateT [t] (StateT Position (MaybeT (Either Err))) a }
                   deriving (Monad, Applicative, Functor, MonadState [t], Alternative, MonadError Err, MonadPlus)

-- newtype Parser t a = Parser {parse :: StateT [t] (StateT Position (ExceptT Err [])) a}
runParser :: Parser t a -> [t] -> Position -> Either Err (Maybe((a,[t]), Position))
runParser p inputString initialPos
  = runMaybeT $ runStateT (runStateT (parse p) inputString) initialPos

--getPosition :: Parser Char [Position]
getPosition :: Parser Char Position
getPosition
  = Parser $ lift get

-- putPosition = lift . put
putPosition :: Position -> Parser Char ()
putPosition
  = Parser . lift . put

updatePosition :: (Position -> Position) -> Parser Char ()
updatePosition f
  = getPosition >>= (putPosition . f)

basicItem :: (MonadState [t] m, MonadPlus m) => m t
basicItem = do
  state <- get
  case state of
    (x:xs) -> do {put xs; return x}
    [] -> mzero

updateParserPosition :: Char -> Position -> Position
updateParserPosition '\n' (ln, c)
  = (ln + 1, 1)
updateParserPosition _ (ln, c)
  = (ln, c + 1)

updateRowPosition :: Position -> Position
updateRowPosition (ln, c) = (ln + 1, c)

errorReporterParser :: (MonadError e m, Alternative m) => m a -> e -> m a
errorReporterParser p err
  = p <|> throwError err

locationReporter :: Parser Char a -> String -> Parser Char a
locationReporter parser errorMessage = do
  p <- getPosition
  errorReporterParser parser ("Syntax Error: " ++ errorMessage, updateRowPosition p)
