module Utility.Declarations where

import Control.Applicative
import Control.Monad

{- TYPE DECLARATIONS -}

newtype Parser a = Parser {parse :: String -> [(a,String)]}

{- INSTANCE DECLARATIONS -}

instance Functor Parser     where
  fmap p q              = q >>= \s -> return (p s)

instance Applicative Parser where
  pure                  = return
  Parser p <*> Parser q = Parser $ \s -> [(f a,s'') | (f,s') <- p s, 
                                                      (a,s'') <- q s']

instance Alternative Parser where
  empty                 = mzero
  p <|> q               = Parser $ \s -> case parse p s of 
                                           []  -> parse q s
                                           res -> res

instance Monad Parser       where
  return a              = Parser $ \s -> [(a,s)]
  p >>= q               = Parser $ \s -> concatMap (\(a,s') -> parse (q a) s') 
                                                   (parse p s)

instance MonadPlus Parser   where
  mzero                 = Parser $ \_ -> []
  mplus p q             = Parser $ \s -> parse p s ++ parse q s
