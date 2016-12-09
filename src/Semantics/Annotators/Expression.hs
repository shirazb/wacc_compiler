{- This module annotates all the identifers found within expressions -}

{-# LANGUAGE MultiWayIf #-}

module Semantics.Annotators.Expression (
  annotateExpr,
  annotateExprList,
  annotateFuncCall,
  annotateMemberAccess
) where

import Control.Monad.State       (get)
import qualified Data.Map as Map

{- LOCAL IMPORTS -}
import Semantics.Annotators.Identifier
import Semantics.Annotators.Util
import Semantics.ErrorMessages
import Utilities.Definitions
import Debug.Trace
import Data.Maybe

-- POST: Annotates expressions
annotateExpr :: Expr -> ScopeAnalysis Expr
annotateExpr (IdentE ident pos) = do
  newIdent <- annotateIdent Variable ident
  return $ IdentE newIdent pos

annotateExpr (ExprArray (ArrayElem ident exprs pos1) pos2) = do
  newIdent <- annotateIdent Variable ident
  newExprs <- annotateExprList exprs
  return $ ExprArray (ArrayElem newIdent newExprs pos1) pos2

annotateExpr (UnaryApp unOp expr pos) = do
  newExpr <- annotateExpr expr
  return $ UnaryApp unOp newExpr pos

annotateExpr (ExprMemberAccess ma pos)
  = ExprMemberAccess <$> annotateMemberAccess ma <*> return pos

annotateExpr (BinaryApp binOp e1 e2 pos) = do
  e1' <- annotateExpr e1
  e2' <- annotateExpr e2
  return $ BinaryApp binOp e1' e2' pos

annotateExpr literal
  = return literal

-- POST: Annotates a list of expressions
annotateExprList :: [Expr] -> ScopeAnalysis [Expr]
annotateExprList
  = mapM annotateExpr

-- POST: Annotates a function call
annotateFuncCall :: FuncCall -> ScopeAnalysis FuncCall
annotateFuncCall (FuncCall f exprList)
  = FuncCall <$> annotateIdent Function f <*> annotateExprList exprList

-- POST: Annotates a member access. From left to right, once a scope error has
--       been hit, the remaining members on the right are left as NoInfo
annotateMemberAccess :: MemberAccess -> ScopeAnalysis MemberAccess
annotateMemberAccess (MemList inst ms pos) = do
    newInst   <- annotateInstance inst
    case identInfo (identOfInst newInst) of
      Info _ (ClassT cname) _ -> do
        newMs <- annotateMemberList cname ms
        return $ MemList newInst newMs pos

      Info _ (FuncT (ClassT cname) _) _ -> do
        newMs <- annotateMemberList cname ms
        return $ MemList newInst newMs pos

      -- First case sets first member to not in scope, as it definitely cannot
      -- the member of a non-class type.
      Info _ t _     -> return $ MemList newInst (setMemberNotInScope
                        (head ms) : tail ms) pos
      ScopeError err -> return $ MemList newInst ms pos
      NoInfo         -> error  $ "annotateMemberAccess: annotatsed instance\
                                 \ near "
                        ++ show pos ++ "has NoInfo. Instance: " ++ show newInst

-- POST: Annotates the identifier of the instance
annotateInstance :: Instance -> ScopeAnalysis Instance
annotateInstance (VarObj ident pos)
  = VarObj <$> annotateIdent Variable ident <*> return pos
annotateInstance (FuncReturnsObj fc pos)
  = FuncReturnsObj <$> annotateFuncCall fc <*> return pos

-- POST: Annotates each member against the object it was called on. The instance
--       is the initial ident to which the member accesses are called on.
annotateMemberList :: String -> [Member] -> ScopeAnalysis [Member]
annotateMemberList _ []
  = error "annotateMemberList: Somehow parsed an empty member list"

annotateMemberList cname [m] = do
  st <- get
  case lookUpIdent (cname, ClassName) st of
    -- Class exists. Check if m is one of cname's members.
    Just ci@(ClassInfo _ is _) -> case [ i | i <- is, m ~= i ] of
      -- m is a member of cname, recurse down the list.
      [i] -> return <$> annotateMember m i
      -- m is not a member of cname.
      []  -> return [setMemberNotInScope m]

      is  -> error $ "annotateMemberList: \ncname: " ++ cname ++
             "\nmember: " ++ show m ++ "\nidents: " ++ show is

    Nothing -> error $ "annotateMemberList: Class name not found: " ++
               show cname

annotateMemberList cname (m : ms) = do
  st <- get
  case lookUpIdent (cname, ClassName) st of
    Just ci@(ClassInfo _ is _) -> case [ i | i <- is, m ~= i ] of
      [i@(Ident _ (Info Instance (ClassT cname') _))] -> do
        newM  <- annotateMember m i
        newMs <- annotateMemberList cname' ms
        return $ newM : newMs

      [i@(Ident _ (Info Instance (FuncT (ClassT cname') _) _))] -> do
        newM  <- annotateMember m i
        newMs <- annotateMemberList cname' ms
        return $ newM : newMs

      [] -> return $ setMemberNotInScope m : ms
      is -> error $ "annotateMemberList: \ncname: " ++ cname ++ "\nmember: " ++
            show m ++ "\nidents: " ++ show is

    Nothing -> error $ "annotateMemberList: Class name not found: " ++
               show cname

-- PRE:  The member is not annotated. The ident is.
-- POST: Annotaes the member's identifier by replacing it with the given one.
--       Annotates the argument list of method calls.
annotateMember :: Member -> Ident -> ScopeAnalysis Member
annotateMember (MethodCall (FuncCall _ es) pos) i = do
  newEs <- annotateExprList es
  return $ MethodCall (FuncCall i newEs) pos
annotateMember m i
  = return $ replaceIdent i m
