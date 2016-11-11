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

-- POST: Outputs a scope error message
mkScopeErrMsg :: ScopeError -> String
mkScopeErrMsg (name, scopeErr, pos)
  = err ++ show scopeErr ++
    (case scopeErr of
      Duplicate  -> "Redeclaration of identifier \'" ++ name ++ "\'."
      NotInScope -> "Identifier \'" ++ name ++ "\' not in scope."
      NoError    -> error "Assertion Failed: found NoError in ident said " ++ 
                      "to have a scope error. ") ++ "Position: " ++ show pos

-- POST: Prepends a string with an assertion 
assertionFailed :: String -> String
assertionFailed                                                                 
  = (++) "Assertion Failed: " 

-- POST: Prepends a strubg with "Error"
errorMsg :: String -> String
errorMsg                                                                        
  = (++) "Error: "

-- POST: Assertion thrown when two symbol tables that are being concatenated
--       do not point to the same enclosing scope
assertSameParentScope :: String
assertSameParentScope                                                           
  = assertionFailed $ "In Semantics.SymbolTable.concatSymTab: " 
    ++ "Attempting to concatenate two symbol tables with different parents" 
    ++ "scopes."

-- POST: Assertion thrown when annotating an identifier that has already been
--       annotated is used in a declaration
assertReannotatingNewIdent :: (Show a1, Show a) => String -> a -> a1 -> String
assertReannotatingNewIdent name currInfo newInfo                                
  = assertionFailed $ "In Semantics.Annotators.Identifier.annotateNewIdent: "   
    ++ "Attempting to reannotate identifier " ++ name ++ " that has info "        
    ++ show currInfo ++ " with " ++ show newInfo ++ "." 

-- POST: Assertion thrown when reannotating an already declared identifier
assertReannotatingIdent :: (Show a1, Show a) => a -> String -> a1 -> String
assertReannotatingIdent ctxt name info                                          
  = assertionFailed $ "In Semantics.Annotators.Identifier.annotateIdent: "      
    ++ "Attempting to reannotate identifier " ++ name ++ "(" ++ show ctxt ++ ")"  
    ++ " that has info " ++ show info ++ "."

-- POST: Assertion thrown when an identifier has no name or context
assertNoNameAndContext :: Show a => a -> String
assertNoNameAndContext ident                                                    
  = assertionFailed $ "In Semantics.Annotators.Util.nameAndContext: "           
    ++ "Attempting to get name and context of identifier " ++ show ident
