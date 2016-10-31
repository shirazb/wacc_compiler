{-
This module defines the parser type which will be used in the rest of the
program. It also defines a number of typeclass instances, enabling us to
benefit from the functions that these typeclasses provide. The functions
provided by the typeclasses are used in defining parser combinators.
Further documentation regarding these typeclasses and their uses can be
found on the Haskell wiki.
-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module Utilities.Declarations where

import Utilities.Definitions
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State       (MonadState (..), StateT (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List                 (nub)
import Utilities.Definitions     (Err, Position)

newtype Parser t a
  = Parser { parse :: StateT [t] (StateT Position (MaybeT (Either Err))) a }
  deriving ( Monad
           , Applicative
           , Functor
           , MonadState [t]
           , Alternative
           , MonadError Err
           , MonadPlus
           )

runParser :: Parser t a -> [t] -> Position ->
               Either Err (Maybe((a,[t]), Position))
runParser p inputString initialPos
  = runMaybeT $ runStateT (runStateT (parse p) inputString) initialPos

getPosition :: Parser Char Position
getPosition
  = Parser $ lift get

putPosition :: Position -> Parser Char ()
putPosition
  = Parser . lift . put

updatePosition :: (Position -> Position) -> Parser Char ()
updatePosition f
  = getPosition >>= (putPosition . f)

basicItem :: (MonadState [t] m, MonadPlus m) => m t
basicItem
  = do
    state <- get
    case state of
      (x:xs) -> do {put xs; return x}
      []     -> mzero

updateParserPosition :: Char -> Position -> Position
updateParserPosition '\n' (ln, c)
  = (ln + 1, 1)
updateParserPosition _ (ln, c)
  = (ln, c + 1)

updateRowPosition :: Position -> Position
updateRowPosition (ln, c)
  = (ln + 1, c)

errorReporterParser :: (MonadError e m, Alternative m) => m a -> e -> m a
errorReporterParser p err
  = p <|> throwError err

tryParser :: Parser Char a -> String -> Parser Char a
tryParser parser errorMessage = do
  p <- getPosition
  errorReporterParser parser
      ("Syntax Error: " ++ errorMessage, updateRowPosition p)
