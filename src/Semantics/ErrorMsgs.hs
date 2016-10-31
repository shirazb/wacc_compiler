module Semantics.ErrorMsgs where

assertionFailed
  = (++) "Assertion Failed: "

errorMsg
  = (++) "Error: "

assertSameParentScope
  = assertionFailed $ "In Semantics.SymbolTable.concatSymTab: "
  ++ "Attempting to concatenate two symbol tables with different parents"
  ++ "scopes."

assertReannotatingNewIdent name currInfo newInfo
  = assertionFailed $ "In Semantics.Annotators.Identifier.annotateNewIdent: "
  ++ "Attempting to reannotate identifier " ++ name ++ " that has info "
  ++ show currInfo ++ " with " ++ show newInfo ++ "."

assertNoNameAndContext ident
  = assertionFailed $ "In Semantics.Annotators.Util.nameAndContext: "
  ++ "Attempting to get name and context of identifier " ++ show ident

errorRedeclaredVar name
  = errorMsg $ "Redeclartion of variable " ++ name ++ "."
