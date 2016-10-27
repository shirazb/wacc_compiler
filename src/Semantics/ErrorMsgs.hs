module Semantics.ErrorMsgs where

assertionFailed
  = (++) "Assertion Failed: "

errorMsg
  = (++) "Error: "

assertSameParentScope
  = assertionFailed $ "In Semantics.SymbolTable.concatSymTab: "
  ++ "Attempting to concatenate two symbol tables with different parents"
  ++ "scopes."

errorRedeclaredVar name
  = errorMsg $ "Redeclartion of variable " ++ name ++ "."
