{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
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


type Position
  = (Int, Int)

type Err
  = (String, Position)

newtype Parser1 a
  = Parser1 { parse1 :: StateT String (StateT Position (MaybeT (Either Err))) a } deriving (Monad, MonadState String, Applicative, Functor, Alternative)

runParser :: Parser1 a -> String -> Either Err (Maybe ((a, String), Position))
runParser p ts
  = runMaybeT $ runStateT (runStateT (parse1 p) ts) (0, 0)


getState :: Parser1 Position
getState
  = Parser1 $ lift get

-- putState :: (MonadState String m) => Position -> Parser1 (m ())
-- putState =  Parser1 $ lift . put

updateState :: (Position -> Position) -> Parser ()
updateState f
  = getState >>= \ x -> lift . put (f x)

basicItem :: (MonadState String m, Alternative m) => m Char
basicItem = do
  state <- get
  case state of
    (x:xs) -> do {put xs; return x}
    [] -> empty

-- item = Parser1 $ do
--   c <- basicItem
--   updateState (f c)
--   return c


f :: Char -> Position -> Position
f d (ln, c)
  = (ln + 1, 0)
f _ (ln, c)
  = (ln , c + 1)


-- basicItem =
--     get >>= \xs -> case xs of
--                         (t:ts) -> put ts *> return t
--                         []     -> return []

commit :: (MonadError m, Alternative m) => Error m -> m a -> m a
commit err p
  = p <|> throwError err

class Monad m => MonadError m where
  type Error m :: *
  throwError :: Error m -> m a
  catchError :: m a -> (Error m -> m a) -> m a

instance MonadError (Either e) where
  type Error (Either e) = e
  throwError               =  Left
  catchError  (Right x) _  =  Right x
  catchError  (Left e)  f  =  f e

instance MonadError m => MonadError (StateT s m) where
  type Error (StateT s m) = Error m
  throwError      =  lift . throwError
  catchError m f  =  StateT g
    where
      g s = catchError (runStateT m s)
                       (\e -> runStateT (f e) s)

instance MonadError m => MonadError (MaybeT m) where
  type Error (MaybeT m) = Error m
  throwError      =  lift . throwError
  catchError m f  =  MaybeT $ catchError (runMaybeT m) (runMaybeT . f)



{- TYPE DECLARATIONS -}

newtype Parser a = Parser {parse :: String -> [(a,String)]}

{- INSTANCE DECLARATIONS -}

{-
The functor typeclass defines a function fmap, which applies the given function to the parsed result of p. Note that the operator <$> is defined as fmap and
is found in the Control.Applicative module.
-}
instance Functor Parser where
  fmap f p              = p >>= \s -> return (f s)

{-
This is the monadic interface defined for our new parser type. It gives us access to the monadic bind function which enables us to chain parsers in sequence.
It also defines the return function which always succeeds and returns the input in a given parser context. Example usage: parse (return 'a') "abc" will return
[('a', "abc")]
-}
instance Monad Parser where
  return a              = Parser $ \s -> [(a,s)]
  p >>= q               = Parser $ \s -> concatMap (\(a,s') -> parse (q a) s')
                                                   (parse p s)
{-
The MonadPlus interface gives us access to two functions. The mzero function is the failure function, it will always fail regardless of the input.
The mplus function is one that takes two parsers as input and applies them both to the input and returns both results as a list.
-}
instance MonadPlus Parser  where
  mzero                 = Parser $ const []
  mplus p q             = Parser $ \s -> parse p s ++ parse q s

{-
The Applicative typeclass defines two functions. The pure function is defined in terms of return which is explained above. The <*> operator takes two Parsers, the first parser
returns as its result a function which is then applied to the result of the second parser.
-}
instance Applicative Parser where
  pure                  = return
  Parser p1 <*> Parser p2 = Parser $ \s -> [(f a,s'') | (f,s') <- p1 s, (a,s'') <- p2 s']

{-
The Alternative typeclass defines two functions. The empty function is defined in terms of mzero. The operator <|> is known as the
choice operator and it takes as input two parsers, it will attempt to parse the given input string using the first parser, iff it fails then it will try that same input with
the second parser. It is a backtracking parser. Note the functions many & some are defined for free in the Alternative typeclass. The function many parses zero or more occurences of a given
parser p and some parses one or more occurences of a given parser p.
-}
instance Alternative Parser where
  empty                 = mzero
  p1 <|> p2               = Parser $ \s -> case parse p1 s of
                                           []  -> parse p2 s
                                           res -> res
