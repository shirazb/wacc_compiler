{-
  This module defines the annotator for classes.
-}
module Semantics.Annotators.Class (
  annotateClass,
  addClassDeclToST
) where

import Control.Monad (liftM3)
import Control.Monad.State.Strict (get, put, lift)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

{-LOCAL IMPORTS-}
-- import Semantics.Annotators.Identifier (annotateNewIdent, annotateIdent)
import Semantics.Annotators.Identifier (annotateNewIdent)
import Semantics.Annotators.Function (annotateFunc, annotateParamList, addFuncDeclToST)
import Semantics.Annotators.Statement (annotateStat)
import Semantics.Annotators.Type
import Semantics.ErrorMessages
import Utilities.Definitions
import Semantics.Annotators.Util
import Debug.Trace

-- PRE : All Class identifiers have been annotated, added to the known types and
--       added to t of the global scope.
--       Cannnot annotate classes that are themselves scope errors.
annotateClass :: Class -> ScopeAnalysis Class
annotateClass c@(Class ident fields constructor methods pos)
  | (ScopeError e) <- identInfo ident = return c
  | otherwise = do

    -- Enter new child scope that contains all of the class' idents
    gs@(ST None _) <- get

    put (mkClassST gs ident)

    let (Ident name _) = ident
    let cType          = ClassT name

    -- Annotate
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
addClassDeclToST (Class ident@(Ident name NoInfo) fields constructor methods pos) = do
  -- Set up dummy child scope of class (so its idents aren't added to global scope)
  globalScope <- get

  put (ST globalScope Map.empty)

  -- Set the info of the members of the class and its constructor
  newFields    <- mapM setFieldInfo fields
  newMethods   <- mapM setMethodInfo methods

  -- Set the ClassInfo, removing duplicate members
  let classMembers = validIdents $ map fieldIdent newFields ++ map funcIdent newMethods
  let constrTypes  = constrType constructor
  let classInfo    = ClassInfo name classMembers constrTypes

  -- Restore global scope and add class to it
  put globalScope

  -- Annotating the ident within the global scope
  -- has the side effect of adding it to the global scope
  newIdent <- annotateNewIdent ident classInfo

  -- Add the constructor to global scope
  let constrT = FuncT (ClassT name) constrTypes
  annotateNewIdent (Ident name NoInfo) (Info Static constrT Function)

  return $ Class newIdent newFields constructor newMethods pos

-- PRE:  The Ident is a class ident that has had its Info set
-- POST: Returns a symbol table whose parent scope is the global scope and whose
--       Env contains all the members of the class
mkClassST :: SymbolTable -> Ident -> SymbolTable
mkClassST gs@(ST None env) (Ident name (ClassInfo _ members _))
  = ST gs envWithSelf
  where
    envWithSelf = Map.insert (self, Variable) (Info Instance (ClassT name) Variable) newEnv
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
-- FIXME: Check class does not take itself in constructor
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

-- -- PRE:  The Params' Idents have been annotated. All class types are known.
-- -- POST: Scope checks the types of each param
-- scopeCheckParamList :: ParamList -> ScopeAnalysis ParamList
-- scopeCheckParamList (ParamList ps pos)
--   = mapM scopeCheckParam ps >>= \ps' -> return (ParamList ps' pos)
--   where
--     scopeCheckParam (Param t i pos')
--       = scopeCheckType t >>= \t' -> return (Param t' i pos')

-- PRE:  The ParamList has had its info set. Its Ident has not already been
--       added to the ST.
-- POST: Annotates the method.
annotateMethod :: Func -> ScopeAnalysis Func
annotateMethod (Func t ident ps body pos) = do
  newT <- scopeCheckType t
  annotateFunc (Func newT ident ps body pos)

-- -- PRE:  The Ident's info has been set
-- -- POST:
-- -- After the first pass, the info of the ident has been set but we have not
-- -- performed scope checking. Idents that are being declared are passed into this
-- -- function, and it will check for reduplication.
-- annotateNewClassIdent :: Ident -> Context -> ScopeAnalysis Ident
-- annotateNewClassIdent i@(Ident name _) ctxt = do
--   st <- get
--   case lookUpIdentCurrScope (name, ctxt) st of
--     Just info  -> return $ Ident name info
--     Nothing    -> error  "annotateNewClassIdent i"
--     -- Nothing -> put newST >> return i

-- PRE:  The Field has no info.
-- POST: Sets the info of the field based on its type
setFieldInfo :: Field -> ScopeAnalysis Field
setFieldInfo (Field t i pos)
  = Field t <$> annotateNewIdent i (Info Instance t Variable) <*> return pos

-- -- PRE:  The Params of the constructor have no info.
-- -- POST: Sets the Info for the parameters of the constructor
-- setConstrParamInfo :: Constructor -> ScopeAnalysis Constructor
-- setConstrParamInfo (Constructor ps body pos) = do
--   newPs <- inChildScope (setParamListInfo ps)
--   return $ Constructor newPs body pos

-- -- PRE:  The ParamList's params have no info
-- -- POST: Sets the Info for the Ident of each paramter based on its type
-- setParamListInfo :: ParamList -> ScopeAnalysis ParamList
-- setParamListInfo (ParamList ps pos)
--   = ParamList <$> mapM setParamInfo ps <*> return pos
--
-- -- PRE:  The Param has no info.
-- -- POST: Sets the Info for the Ident of a paramter based on its type
-- setParamInfo :: Param -> ScopeAnalysis Param
-- setParamInfo (Param t i pos)
--   = Param t <$> annotateNewIdent i (Info t Variable) <*> return pos

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
