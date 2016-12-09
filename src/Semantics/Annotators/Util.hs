{- This module defines a number of utility functions which are used whilst
annotating the AST -}

-- TODO : abstract out the symbol table get

module Semantics.Annotators.Util where

import qualified Data.Map as Map
import Control.Monad.State.Strict (get, put, State (..), lift, runState)
import Data.Maybe (fromJust)
import Debug.Trace

{- LOCAL IMPORTS -}
import Semantics.ErrorMessages
import Utilities.Definitions

type ScopeAnalysis a  = State SymbolTable a
type Env              = Map.Map (String, Context) Info

data SymbolTable
  = ST SymbolTable Env
  | None
  deriving (Eq, Show)

runScopeAnalysis :: ScopeAnalysis a -> a
runScopeAnalysis analyser
  = ast
  where
    (ast, _) = runState analyser (ST None Map.empty)
--
-- -- POST: Add a new type to the list
-- addKnownType :: String -> ScopeAnalysis ()
-- addKnownType t = do
--   ts <- knownTypes
--   lift . put $ (t:ts)

-- POST: Returns the identifier of a param
paramIdent :: Param -> Ident
paramIdent (Param _ i _)
  = i

-- POST: Returns the context of an Info
context :: Info -> Context
context (Info _ _ ctxt)
  = ctxt
context ClassInfo{}
  = ClassName
context c
  = error $ "Cannot call context on " ++ show c

-- PRE:  The member in not annotated. The ident is.
-- POST: Checks if the member matches the Ident.
(~=) :: Member -> Ident -> Bool
FieldAccess (Ident n NoInfo) _ ~= Ident n' (Info Instance _ Variable)
  = n == n'
MethodCall (FuncCall (Ident n NoInfo) _) _ ~= Ident n' (Info Instance _ Function)
  = n == n'
_ ~= _
  = False

-- POST: Returns the identifier of the instance
identOfInst :: Instance -> Ident
identOfInst (VarObj i _)
  = i
identOfInst (FuncReturnsObj (FuncCall i _) _)
  = i

-- POST: Returns true if the given identifier is marked with an error.
identHasError :: Ident -> Bool
identHasError (Ident _ (ScopeError _))
  = True
identHasError (Self (ScopeError _))
  = True
identHasError _
  = False

self :: String
self
  = "self"

-- POST: Sets the info of a given identifier
setInfo :: Info -> Ident -> Ident
setInfo info (Ident name _)
  = Ident name info
setInfo info (Self _)
  = Self info

-- POST: Adds given identifier to the given map.
addToEnv :: Ident -> Env -> Env
addToEnv ident
  = Map.insert (nameAndContext ident) (identInfo ident)

-- POST: identifiers are added to the environment
populateEnvironment :: Env -> [Ident] -> Env
populateEnvironment
  = foldl $ flip addToEnv


-- PRE:  The Field's Idents have been annotated
-- POST: Returns all the fields that do not have a scope error
validIdents :: [Ident] -> [Ident]
validIdents
  = filter (not . identHasError)

-- POST: Adds the identifer to the symbol table.
addToST :: Ident -> SymbolTable -> SymbolTable
addToST ident (ST parent env)
  = ST parent (addToEnv ident env)
addToST _ _
  = error $ assertionFailed "Cannot add non annotated or error" ++
      " identifer to the symbol table."

-- POST: Retrieves the info of the given input identifier
identInfo :: Ident -> Info
identInfo (Ident _ info)
  = info
identInfo (Self info)
  = info

-- POST:: Retrieves the ident of the Func
funcIdent :: Func -> Ident
funcIdent (Func _ ident _ _ _)
  = ident

-- POST: Returns the name and context of an annotated identifier
nameAndContext :: Ident -> (String, Context)
nameAndContext (Ident name (Info _ _ context))
  = (name, context)
nameAndContext (Ident name ClassInfo{})
  = (name, ClassName)
nameAndContext (Self _)
  = (self, Variable)
nameAndContext ident
  = error $ assertNoNameAndContext ident

-- POST: Returns the type information from info
typeInfo :: Info -> Type
typeInfo (Info _ t _)
  = t
typeInfo (ClassInfo name _ _)
  = ClassT name
typeInfo i
  = error $ "Cannot call typeInfo on \'" ++ show i ++ "\'"

-- POST: Gets the members from the ClassInfo
members :: Info -> [Ident]
members (ClassInfo _ ms _)
  = ms
members i
  = error $ "Cannot call members on \'" ++ show i ++ "\'"

-- POST: Returns the types of all params in a ParamList
paramTypes :: ParamList -> [Type]
paramTypes (ParamList ps _)
  = map (\(Param t i _) -> t) ps

-- POST: Sets the error type of the identifer
setErrType :: ScopeErrorType -> Ident -> Ident
setErrType errType i@(Ident name _)
  = Ident name (ScopeError errType)

memberIdent :: Member -> Ident
memberIdent (FieldAccess i pos)
  = i
memberIdent (MethodCall (FuncCall i _) _)
  = i

memberInfo :: Member -> Info
memberInfo
  = identInfo . memberIdent

-- POST: Sets the error type of identifier in the member
setMemberNotInScope :: Member -> Member
setMemberNotInScope m@(FieldAccess i pos)
  = replaceIdent (setErrType NotInScope i) m
setMemberNotInScope m@(MethodCall (FuncCall i es) pos)
  = replaceIdent (setErrType NotInScope i) m

  -- POST: Replaces the identifer of the member with the given one
replaceIdent :: Ident -> Member -> Member
replaceIdent i (FieldAccess _ pos)
  = FieldAccess i pos
replaceIdent i (MethodCall (FuncCall _ es) pos)
  = MethodCall (FuncCall i es) pos

-- POST: Returns information about a given identifier if it is in the
--       the current scope of the symbol table
lookUpIdentCurrScope :: (String, Context) -> SymbolTable -> Maybe Info
lookUpIdentCurrScope nameAndCtxt (ST _ env)
  = Map.lookup nameAndCtxt env

-- -- PRE: Ident is a class
-- -- POST: Returns the members of this class
-- availableMembers (Ident _ (ClassInfo _ mems _))
--   = mems
-- availableMembers i
--   = error $ "Cannot call availableMembers on the identifer: " ++ show i

-- POST: Returns information about a given identifier if it is in
--       any scope of the symbol table
lookUpIdent :: (String, Context) -> SymbolTable -> Maybe Info
lookUpIdent _ None
  = error "write a proper error msg -- lookUpIdent"
lookUpIdent nameAndCtxt st@(ST None env)
  = Map.lookup nameAndCtxt env
lookUpIdent nameAndCtxt (ST parentST env)
  = case Map.lookup nameAndCtxt env of
    Nothing   -> lookUpIdent nameAndCtxt parentST
    Just info -> Just info

-- POST: Peforms a given annotator inside of a
--       child scope. Exits to the parent scope
inChildScope :: ScopeAnalysis a -> ScopeAnalysis a
inChildScope child = do
  parentST <- get
  put (ST parentST Map.empty)
  annotatedAST <- child
  put parentST
  return annotatedAST

-- POST: Same as above but applys a function to the result of annotation
inChildScopeAndWrap :: (a -> b) -> ScopeAnalysis a -> ScopeAnalysis b
inChildScopeAndWrap f child
  = f <$> inChildScope child
