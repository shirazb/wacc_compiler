{- This module defines the annotator for classes -}

module Semantics.Annotators.Class (
  annotateClass,
  addClassDeclToST
) where

import Control.Monad                   (liftM3)
import Control.Monad.State.Strict      (get, put, lift)
import Data.Maybe                      (fromJust)
import qualified Data.Map as Map

{-LOCAL IMPORTS-}
import Semantics.Annotators.Identifier (annotateNewIdent)
import Semantics.Annotators.Function   (annotateFunc, annotateParamList,
                                        addFuncDeclToST)
import Semantics.Annotators.Statement  (annotateStat)
import Semantics.Annotators.Type
import Semantics.ErrorMessages
import Utilities.Definitions
import Semantics.Annotators.Util
import Debug.Trace

-- PRE: All Class identifiers have been annotated, added to the known types and
--      added to t of the global scope.
--      Cannnot annotate classes that are themselves scope errors.
annotateClass :: Class -> ScopeAnalysis Class
annotateClass c@(Class ident fields constructor methods pos)
  | (ScopeError e) <- identInfo ident = return c
  | otherwise = do
    -- Enter new child scope that contains all of the class' idents
    gs@(ST None _) <- get
    put (mkClassST gs ident)
    let (Ident name _) = ident
    let cType          = ClassT name
    newConstr  <- annotateConstr cType constructor
    newFields  <- mapM annotateField fields
    newMethods <- mapM annotateMethod methods
    -- Leave class scope; restore global scope
    put gs
    return $ Class ident newFields newConstr newMethods pos

-- PRE:  The Class is unannotated and we are in global scope.
-- POST: Annotates the fields, constructor and method declarations. From this,
--       annotates the class' Ident and adds it to the list of known types.
addClassDeclToST :: Class -> ScopeAnalysis Class
addClassDeclToST (Class ident@(Ident name NoInfo) fields constructor
  methods pos) = do
  globalScope     <- get
  put (ST globalScope Map.empty)
  newFields       <- mapM setFieldInfo fields
  newMethods      <- mapM setMethodInfo methods
  let classMembers = validIdents $ map fieldIdent newFields ++
                     map funcIdent newMethods
  let constrTypes  = constrType constructor
  let classInfo    = ClassInfo name classMembers constrTypes
  put globalScope
  newIdent        <- annotateNewIdent ident classInfo
  let constrT      = FuncT (ClassT name) constrTypes
  annotateNewIdent (Ident name NoInfo) (Info Static constrT Function)
  return $ Class newIdent newFields constructor newMethods pos

-- PRE:  The Ident is a class ident that has had its Info set
-- POST: Returns a symbol table whose parent scope is the global scope and whose
--       Env contains all the members of the class
mkClassST :: SymbolTable -> Ident -> SymbolTable
mkClassST gs@(ST None env) (Ident name (ClassInfo _ members _))
  = ST gs envWithSelf
  where
    envWithSelf = Map.insert (self, Variable) (Info Instance (ClassT name)
                  Variable) newEnv
    newEnv      = populateEnvironment env members
mkClassST gs i
  = error $ "Annotators.Class.mkClassST: Invalid Args\n" ++
      "    Global Scope: " ++ show gs ++ "\n" ++
      "           Ident: " ++ show i ++ "\n"

-- PRE:  The Field's info has been set. Its Ident has not already been added to
--       the ST.
-- POST: Annotates the field. This is checking that the type is in scope.
annotateField :: Field -> ScopeAnalysis Field
annotateField (Field t i pos) = do
    newType  <- scopeCheckType t
    return (Field newType i pos)

-- PRE:  The Constructor has had its info set
-- POST: Annotates the constructor
annotateConstr :: Type -> Constructor -> ScopeAnalysis Constructor
annotateConstr t (Constructor params body pos)
 = inChildScope (
     liftM3 Constructor
       (annotateParamList params *> checkCyclicConstr t params)
       (annotateStat body)
       (return pos)
  )

checkCyclicConstr :: Type -> ParamList -> ScopeAnalysis ParamList
checkCyclicConstr t@(ClassT n) (ParamList ps pos)
  = return $ ParamList ps' pos
  where
    ps' = map (\(Param t' i pos') ->
        if t == t'
          then Param t' (setErrType (CyclicConstr n) i ) pos'
          else Param t' i pos') ps

-- PRE:  The ParamList has had its info set. Its Ident has not already been
--       added to the ST
-- POST: Annotates the method
annotateMethod :: Func -> ScopeAnalysis Func
annotateMethod (Func t ident ps body pos) = do
  newT <- scopeCheckType t
  annotateFunc (Func newT ident ps body pos)

-- PRE:  The Field has no info.
-- POST: Sets the info of the field based on its type
setFieldInfo :: Field -> ScopeAnalysis Field
setFieldInfo (Field t i pos)
  = Field t <$> annotateNewIdent i (Info Instance t Variable) <*> return pos

-- PRE:  The Params of the method have no info
-- POST: Sets the Info for the parameters of the method
setMethodInfo :: Func -> ScopeAnalysis Func
setMethodInfo (Func t ident params body pos) = do
  newIdent  <- annotateNewIdent ident (Info Instance t Function)
  return $ Func t newIdent params body pos

-- POST: Returns the types of the parameter of the given constructor
constrType :: Constructor -> [Type]
constrType (Constructor (ParamList params _) body pos)
  = map (\(Param t i _) -> t) params
