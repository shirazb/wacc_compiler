{- This module generates ARM Assembly code for AssignRHS -}

module CodeGen.AssignRHS where

import Control.Monad.StateStack
import Control.Monad.State.Strict (get, put, lift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import CodeGen.Expression
import Utilities.Definitions

instance CodeGen AssignRHS where
  codegen (ExprAssign e _)
    = codegen e
  codegen (ArrayLitAssign exprs _) = do
    let sizeArrayObj = 4 + sum (map exprSize exprs)
    let mallocArray  = malloc sizeArrayObj
    populateArray    <- moveExprsIntoArray exprs 4
    return $
      mallocArray ++
      [Mov R3 R0] ++
      populateArray ++
      [Mov R0 (ImmI (length exprs))] ++
      [STR W NoIdx R0 [R3]] ++
      [Mov R0 R3]
    where
      moveExprsIntoArray :: [Expr] -> Int -> InstructionMonad [Instr]
      moveExprsIntoArray [] _
        = return []
      moveExprsIntoArray (e : es) offset = do
        evalE         <- codegen e
        let size      = sizeFromType (typeOfExpr e)
        let newOffset = offset + exprSize e
        moveRemaining <- moveExprsIntoArray es newOffset
        return $
          evalE ++
          [STR size NoIdx R0 [R3, (ImmI offset)]] ++
          moveRemaining
  codegen NewPairAssign{}
    = undefined
  codegen (FuncCallAssign ident@(Ident name info) es _) = do
    let FuncT retType paramTypes = typeInfo info
    params <- mapM codegen es
    let pushParams = concat $ reverse $ zipWith (\p t -> p ++ pushParam t) params paramTypes
    let callFunc = [BL name]
    let paramSpace  = sum (map typeSize paramTypes)
    let clearParams = [ADD NF SP SP (ImmOp2 paramSpace)]
    return $ pushParams ++ callFunc ++ clearParams
    where
      pushParam :: Type -> [Instr]
      pushParam t
        = [STR size Pre R0 [SP, ImmI (- typeSize t)]]
        where
          size = sizeFromType t

-- Gens instrs to malloc the given amount
malloc :: Int -> [Instr]
malloc n
  = [Mov R0 (ImmI n)] ++ [BL "malloc"]
