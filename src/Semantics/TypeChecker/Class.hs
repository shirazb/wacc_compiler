{-
  There are three things about the constructor that must be checked.
    1. Fields are not used before initialisation,
    2. All fields are initialised by the end of the constructor,
    3. No method calls are made until all fields have been initialised.
-}

module Semantics.TypeChecker.Class (typeCheckClass) where

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

{- LOCAL IMPORTS -}
import Semantics.TypeChecker.Statement (typeCheckStat)
import Semantics.TypeChecker.Function (typeCheckFunc)
import Semantics.Annotators.Util (identInfo)
import Semantics.ErrorMessages
import Utilities.Definitions

typeCheckClass :: Class -> TypeChecker ()
typeCheckClass (Class ident fields constr@(Constructor _ body pos) methods _) = do
  typeCheckConstr constr
  mapM_ typeCheckFunc methods
  mapM_ (checkFieldInitialisedIn ident pos body) fields
  let errs = map (runCheck body . (\(Field _ i _) -> i)) fields
  mapM_ tell errs

typeCheckConstr :: Constructor -> TypeChecker ()
typeCheckConstr (Constructor _ body _)
  = typeCheckStat body

-- POST: Checks the field is not used until it is initialised, and that all
--       fields are initialised
checkFieldInitialisedIn :: Ident -> Position -> Stat -> Field -> TypeChecker ()
checkFieldInitialisedIn ident pos body f
  | searchForAssignment body f = return ()
  | otherwise                  = tell [unassignedField ident pos f]

-- checkField' :: Stat -> Field -> [Field] -> (Bool, [Field])
-- checkField' (Assignment lhs rhs _) f fs
--   = if isF
--       then (True && validRHS, f : fs)
--       else (False, fs)
--   where
--     isF      = identInfo (fieldIdent f) == identInfo (lhsIdent lhs)
--     validRHS = checkUseInRHS rhs
-- checkField' (If e s s' _) f fs
--   = (b && b' && validExpr, inited)
--   where
--     inited    = filter (`elem` fs') fs
--     (b, fs)   = checkField' s f fs
--     (b', fs') = checkField' s' f fs
--     validExpr = checkUseInExpr e fs
-- checkField' (While )

tellE = lift . tell

runCheck :: Stat -> Ident -> [String]
runCheck s f
  = errs
  where
    (_, errs) = runWriter (runStateT (checkUseInStat s f) False)

-- Checks field not used before initialisation
checkUseInStat :: Stat -> Ident -> StateT Bool (Writer [String]) ()
checkUseInStat (Declaration _ i rhs _) i'
  = checkUseInRHS rhs i'
checkUseInStat (Assignment lhs rhs _) i' = do
  checkUseInRHS rhs i'
  let isField = identInfo i' == identInfo (lhsIdent lhs)
  when isField (put True)
checkUseInStat (Read lhs _) i
  = checkUseInLHS lhs i
checkUseInStat (Free e _) i
  = checkUseInExpr e i
checkUseInStat (Return e _) i
  = checkUseInExpr e i
checkUseInStat (Exit e _) i
  = checkUseInExpr e i
checkUseInStat (Print e _) i
  = checkUseInExpr e i
checkUseInStat (Println e _) i
  = checkUseInExpr e i
checkUseInStat (If e s s' _) i = do
  checkUseInExpr e i
  checkUseInStat s i
  checkUseInStat s' i
checkUseInStat (While e s _) i = do
  checkUseInExpr e i
  checkUseInStat s i
checkUseInStat (For s e s' s'' _) i = do
  checkUseInStat s i
  checkUseInExpr e i
  checkUseInStat s' i
  checkUseInStat s'' i
checkUseInStat (Block s _) i
  = checkUseInStat s i
checkUseInStat (CallFunc fc pos) i
  = checkUseInFC fc i pos
checkUseInStat (CallMethod ma _) i
  = checkUseInMA ma i
checkUseInStat (Seq s s' _) i = do
  checkUseInStat s i
  checkUseInStat s' i
checkUseInStat _ _
  = return ()

checkUseInIdent :: Ident -> Ident -> Position -> StateT Bool (Writer [String]) ()
checkUseInIdent i i' pos = do
  initd <- get
  when (i == i' && not initd) (tellE [uninitdUse i pos])

checkUseInLHS :: AssignLHS -> Ident -> StateT Bool (Writer [String]) ()
checkUseInLHS (Var i' pos) i
  = checkUseInIdent i' i pos
checkUseInLHS (ArrayDeref (ArrayElem i' es pos') pos) i = do
  checkUseInIdent i' i pos'
  mapM_ (`checkUseInExpr` i) es
checkUseInLHS (MemberDeref ma _) i
  = checkUseInMA ma i

checkUseInRHS :: AssignRHS -> Ident -> StateT Bool (Writer [String]) ()
checkUseInRHS (ExprAssign e _) i
  = checkUseInExpr e i
checkUseInRHS (ArrayLitAssign es _) i
  = mapM_ (`checkUseInExpr` i) es
checkUseInRHS (NewPairAssign e e' _) i
  = checkUseInExpr e i >> checkUseInExpr e' i
checkUseInRHS (PairElemAssign (PairElem _ e _) _) i
  = checkUseInExpr e i
checkUseInRHS (FuncCallAssign fc pos) i
  = checkUseInFC fc i pos
checkUseInRHS (ConstructAssign fc pos) i
  = checkUseInFC fc i pos

checkUseInExpr :: Expr -> Ident -> StateT Bool (Writer [String]) ()
checkUseInExpr (ExprMemberAccess ma _) i
  = checkUseInMA ma i
checkUseInExpr (IdentE i' pos) i
  = checkUseInIdent i' i pos
checkUseInExpr (ExprArray (ArrayElem i' es pos) _) i = do
  checkUseInIdent i i' pos
  mapM_ (`checkUseInExpr` i) es
checkUseInExpr (UnaryApp _ e _) i
  = checkUseInExpr e i
checkUseInExpr (BinaryApp _ e e' _) i
  = checkUseInExpr e i >> checkUseInExpr e' i
checkUseInExpr _ _
  = return ()

checkUseInFC :: FuncCall -> Ident -> Position -> StateT Bool (Writer [String]) ()
checkUseInFC (FuncCall i' es) i pos = do
  checkUseInIdent i' i pos
  mapM_ (`checkUseInExpr` i) es

checkUseInMA :: MemberAccess -> Ident -> StateT Bool (Writer [String]) ()
checkUseInMA (MemList inst ms pos) i = do
  checkUseInInst inst i
  mapM_  (`checkUseInMem` i) ms

checkUseInInst :: Instance -> Ident -> StateT Bool (Writer [String]) ()
checkUseInInst (VarObj i' pos) i
  = checkUseInIdent i' i pos
checkUseInInst (FuncReturnsObj fc pos) i
  = checkUseInFC fc i pos

checkUseInMem :: Member -> Ident -> StateT Bool (Writer [String]) ()
checkUseInMem (FieldAccess i' pos) i
  = checkUseInIdent i' i pos
checkUseInMem (MethodCall fc pos) i
  = checkUseInFC fc i pos

-- Checks if field never initialised in stat
searchForAssignment :: Stat -> Field -> Bool
searchForAssignment (Assignment lhs _ _) f
  = identInfo (fieldIdent f) == identInfo (lhsIdent lhs)
searchForAssignment (If e s s' _) f
  = searchForAssignment s f && searchForAssignment s' f
searchForAssignment While{} f
  = False
searchForAssignment For{} f
  = False
searchForAssignment (Block s _) f
  = searchForAssignment s f
searchForAssignment (Seq s s' _) f
  = searchForAssignment s f || searchForAssignment s' f
searchForAssignment _ _
  = False

lhsIdent :: AssignLHS -> Ident
lhsIdent (Var i _)
  = i
lhsIdent (MemberDeref (MemList _ ms _) _)
  = i
  where
    (FieldAccess i _) = last ms
-- Not an ident -- return dummy ident that cannot be matched by a field
lhsIdent _
  = Ident "" (Info Static Void Function)
