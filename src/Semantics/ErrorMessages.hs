{- This module defines semantic error messages to be printed out -}

module Semantics.ErrorMessages where

{- LOCAL IMPORTS -}
import Utilities.Definitions

err, loc, expe, act, expr        :: ErrorMsg
typeMM, arrayDimMM, derefNullPtr :: ErrorMsg
numOfArgs, freeNonHeap           :: ErrorMsg

err          = "   Semantic Error: "
loc          = "      At location: "
expe         = "    Expected type: "
act          = "      Actual type: "
expr         = "In the expression: "

typeMM       = "Type mismatch"
arrayDimMM   = "Array dimension mismatch"
derefNullPtr = "Dereferencing a null pointer"
numOfArgs    = "Incorrect number of arguments"
freeNonHeap  = "Freeing non heap allocated object"
returnMain   = "Cannot return from the top-level statement of the program"
noLoopBreak  = "No enclosing loop to break out of"
noLoopCont   = "No enclosing loop to continue with"
divByZero    = "Divide By Zero Error -- constant expression evalutes to zero"
overFlowErr  = "OverFlow Error -- constant expression overflows"

-- POST: Outputs a type mismatch error message
typeMismatch :: Show a => Type -> Type -> Position -> a -> ErrorMsg
typeMismatch expT actT pos expr'
  = err  ++ typeMM     ++ "\n" ++
    loc  ++ show pos   ++ "\n" ++
    expe ++ show expT  ++ "\n" ++
    act  ++ show actT  ++ "\n" ++
    expr ++ show expr' ++ "\n"

-- POST: Outputs an array dimension mismatch error message
dimensionMismatch :: Show a => Int -> Int -> Position -> a -> ErrorMsg
dimensionMismatch expDim actDim pos arrElem
  = err  ++ arrayDimMM     ++ "\n" ++
    loc  ++ show pos       ++ "\n" ++
    expe ++ "An array of at least" ++ show expDim ++ "dimensions" ++ "\n" ++
    act  ++ "An array of "         ++ show actDim ++ "dimensions" ++ "\n" ++
    expr ++ show arrElem   ++ "\n"

-- POST: Outputs an error if a null pointer is being dereferenced
nullPtrDeref :: Show a => Position -> a -> ErrorMsg
nullPtrDeref pos expr'
  = err  ++ derefNullPtr ++ "\n" ++
    loc  ++ show pos     ++ "\n" ++
    expr ++ show expr'   ++ "\n"

-- POST: Outputs a list type mismatch error message
typeMismatchList :: Show a => [Type] -> [Type] -> Position -> a -> ErrorMsg
typeMismatchList expT actT pos expr'
  = err  ++ typeMM     ++ "\n" ++
    loc  ++ show pos   ++ "\n" ++
    expe ++ show expT  ++ "\n" ++
    act  ++ show actT  ++ "\n" ++
    expr ++ show expr' ++ "\n"

-- POST: Outputs an error if we have divide by zero
divideByZero :: Position -> Expr -> ErrorMsg
divideByZero pos e
  = err  ++ divByZero ++ "\n" ++
    loc  ++ show pos  ++ "\n" ++
    expr ++ show e    ++ "\n"

-- POST: Outputs an error if we have overflow
overFlowError :: Position -> Expr -> ErrorMsg
overFlowError pos e
  = err  ++ overFlowErr ++ "\n" ++
    loc  ++ show pos    ++ "\n" ++
    expr ++ show e      ++ "\n"

-- POST: Outputs an error message if freeing a non-heap allocated object
freeNonHeapObject :: Position -> ErrorMsg
freeNonHeapObject pos
  = err  ++ freeNonHeap ++ "\n" ++
    loc  ++ show pos    ++ "\n"

-- POST: Outputs an error message if returning from the top-level statement of
--       the program
returnInMain :: ErrorMsg
returnInMain
  = err ++ returnMain ++ "\n"

breakWithoutLoop :: ErrorMsg
breakWithoutLoop
  = err ++ noLoopBreak ++ "\n"

continueWithoutLoop :: ErrorMsg
continueWithoutLoop
  = err ++ noLoopCont ++ "\n"

-- POST: Outputs a scope error message
mkScopeErrMsg :: ScopeError -> ErrorMsg
mkScopeErrMsg (name, scopeErr, pos)
  = err ++
    (case scopeErr of
      Duplicate  -> "Redeclaration of identifier \'" ++ name
      NotInScope -> "Identifier \'" ++ name ++ "\' not in scope."
      NoError    -> error "Assertion Failed: found NoError in ident said " ++
                      "to have a scope error. ") ++
                      "\n         Position: " ++ show pos

-- POST: Prepends a string with an assertion
assertionFailed :: String -> ErrorMsg
assertionFailed
  = (++) "Assertion Failed: "

-- POST: Prepends a strubg with "Error"
errorMsg :: String -> ErrorMsg
errorMsg
  = (++) "Error: "

-- POST: Assertion thrown when two symbol tables that are being concatenated
--       do not point to the same enclosing scope
assertSameParentScope :: ErrorMsg
assertSameParentScope
  = assertionFailed $ "In Semantics.SymbolTable.concatSymTab: "
    ++ "Attempting to concatenate two symbol tables with different parents"
    ++ "scopes."

-- POST: Assertion thrown when annotating an identifier that has already been
--       annotated is used in a declaration
assertReannotatingNewIdent :: (Show a1, Show a) => String -> a -> a1
                                -> ErrorMsg
assertReannotatingNewIdent name currInfo newInfo
  = assertionFailed $ "In Semantics.Annotators.Identifier.annotateNewIdent: "
    ++ "Attempting to reannotate identifier " ++ name ++ " that has info "
    ++ show currInfo ++ " with " ++ show newInfo ++ "."

-- POST: Assertion thrown when reannotating an already declared identifier
assertReannotatingIdent :: (Show a1, Show a) => a -> String -> a1 -> ErrorMsg
assertReannotatingIdent ctxt name info
  = assertionFailed $ "In Semantics.Annotators.Identifier.annotateIdent: "
    ++ "Attempting to reannotate identifier " ++ name ++ "(" ++ show ctxt ++ ")"
    ++ " that has info " ++ show info ++ "."

-- POST: Assertion thrown when an identifier has no name or context
assertNoNameAndContext :: Show a => a -> ErrorMsg
assertNoNameAndContext ident
  = assertionFailed $ "In Semantics.Annotators.Util.nameAndContext: "
    ++ "Attempting to get name and context of identifier " ++ show ident
