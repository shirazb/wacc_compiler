module Semantics.ErrorMessages where

import Utilities.Definitions

err       = "   Semantic Error: "
loc       = "      At location: "
expe      = "    Expected type: "
act       = "      Actual type: "
expr      = "In the expression: "

msg1      = "Type mismatch"
msg2      = "Array dimension mismatch"
msg3      = "Dereferencing a null pointer"
msg4      = "Incorrect number of arguments"
msg5      = "Freeing non heap allocated object"

typeMismatch :: Show a => Type -> Type -> Position -> a -> ErrorMsg
typeMismatch expT actT pos expr'
  = err ++ msg1 ++ "\n" ++
    loc ++ show pos ++ "\n" ++
    expe ++ show expT ++ "\n" ++
    act ++ show actT ++ "\n" ++
    expr ++ show expr' ++ "\n"

dimensionMismatch :: Show a => Int -> Int -> Position -> a -> ErrorMsg
dimensionMismatch expDim actDim pos arrElem
  = err ++ msg2 ++ "\n" ++
    loc ++ show pos ++ "\n" ++
    expe ++ "An array of at least" ++ show expDim ++ "dimensions" ++ "\n" ++
    act ++ "An array of " ++ show actDim ++ "dimensions" ++ "\n" ++
    expr ++ show arrElem ++ "\n"

nullPtrDeref :: Show a => Position -> a -> ErrorMsg
nullPtrDeref pos expr'
  = err ++ msg3 ++ "\n" ++
    loc ++ show pos ++ "\n" ++
    expr ++ show expr' ++ "\n"

typeMismatchList :: Show a => [Type] -> [Type] -> Position -> a -> ErrorMsg
typeMismatchList expT actT pos expr'
  = err ++ msg1 ++ "\n" ++
    loc ++ show pos ++ "\n" ++
    expe ++ show expT ++ "\n" ++
    act ++ show actT ++ "\n" ++
    expr ++ show expr' ++ "\n"

mkScopeErrMsg :: ScopeError -> String
mkScopeErrMsg (name, scopeErr, pos)
  = err ++ show scopeErr ++
    (case scopeErr of
      Duplicate  -> "Redeclaration of identifier \'" ++ name ++ "\'."
      NotInScope -> "Identifier \'" ++ name ++ "\' not in scope."
      NoError    -> error "Assertion Failed: found NoError in ident said " ++ 
                      "to have a scope error. ") ++ "Position: " ++ show pos
