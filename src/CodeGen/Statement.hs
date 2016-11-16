{- This module generates ARM Assembly code for statements -}

module CodeGen.Statement where

{- LOCAL IMPORTS -}
import CodeGen.Assembly
import Utilities.Definitions
import Control.Monad.State(get, put, lift)
import qualified Data.Map as Map
import Control.Monad.StateStack
import Data.Maybe (fromJust)


instance CodeGen Stat where
  codegen (Declaration t ident@(Ident name _) rhs _) = do
    instr <- codegen rhs
    (map', offset) <- get
    let newMap = Map.insert name offset map'
    let newOffset = offset + typeSize t
    put (newMap, newOffset)
    let str = [STR W NoIdx R0 [SP,ImmI offset]]
    return $ instr ++ str
  codegen (Block s _) = do
    let sizeOfscope = scopeSize s
    (map', offset) <- get
    let newMap = Map.map (+ sizeOfscope) map'
    save
    put (newMap, 0)
    let assembleSpace = [SUB SP SP (ImmI sizeOfscope)]
    instr <- codegen s
    let clearSpace = [ADD SP SP (ImmI sizeOfscope)]
    restore
    return $ assembleSpace ++ instr ++ clearSpace
  codegen (Seq s1 s2 _) = do
    instr1 <- codegen s1
    instr2 <- codegen s2
    return $ instr1 ++ instr2


instance CodeGen AssignRHS where
  codegen (ExprAssign e _)
    = codegen e
  codegen (ArrayLitAssign expr _)
    = undefined
  codegen NewPairAssign{}
    = undefined
  codegen (FuncCallAssign ident@(Ident name info) es _) = do
    let FuncT retType paramTypes = typeInfo info
    params <- mapM codegen es
    let pushParams = concat $ reverse $ zipWith (\p t -> p ++ pushParam t) params paramTypes
    let callFunc = [BL name]
    let paramSpace  = sum (map typeSize paramTypes)
    let clearParams = [ADD SP SP (ImmI paramSpace)]
    return $ pushParams ++ callFunc ++ clearParams
    where
      pushParam :: Type -> [Instr]
      pushParam t
        = [STR size Pre R0 [SP, ImmI (- typeSize t)]]
        where
          size = sizeFromType t

instance CodeGen Expr where
  codegen (IntLit i _)
    = return [LDR W NoIdx R0 [ImmLDRI i]]
  codegen (CharLit c _)
    = return [LDR W NoIdx R0 [ImmLDRC c]]
  codegen (BoolLit b _)
    = return [Mov R0 (ImmI bInt)]
    where
      bInt = if b then 1 else 0
  codegen (PairLiteral _)
    = return [Mov R0 (ImmI 0)]
  codegen (IdentE (Ident name (Info t _)) _) = do
    let size = sizeFromType t
    (env, _) <- get
    let offset = fromJust $ Map.lookup name env
    return [LDR size NoIdx R0 [SP, ImmI offset]]
  codegen (ExprArray _ _)
    = undefined
  codegen (UnaryApp Not e _) = do
    instr <- codegen e
    let notE = [EOR R0 R0 (ImmI 1)]
    return $ instr ++ notE
  codegen (UnaryApp Neg e _) = do
    instr <- codegen e
    let negE = [RSBS R0 R0 (ImmI 0)]
    return $ instr ++ negE
  codegen (UnaryApp Len e _) = do
    instr <- codegen e
    let getLen = [LDR W NoIdx R0 [R0]]
    return $ instr ++ getLen
  codegen (UnaryApp Ord e _) = do
    return []
instance CodeGen Func where
  codegen _
    = return []

scopeSize :: Stat -> Int
scopeSize (Declaration t _ _ _)
  = typeSize t
scopeSize (Seq s1 s2 _)
  = scopeSize s1 + scopeSize s2
scopeSize _
  = 0

typeSize :: Type -> Int
typeSize (BaseT BaseInt)
  = 4
typeSize (BaseT BaseBool)
  = 1
typeSize (BaseT BaseChar)
  = 1
typeSize _
  = error "not yet implemented"

sizeFromType :: Type -> Size
sizeFromType
  = fromJust . flip lookup typeSizes
