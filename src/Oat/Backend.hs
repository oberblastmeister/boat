{-# LANGUAGE TemplateHaskell #-}

module Oat.Backend where

import qualified Control.Lens as L
import Control.Lens.Operators
import Oat.Alloc (Loc (..), Operand (..))
import qualified Oat.Alloc as Alloc
import Oat.Common ((++>))
import qualified Oat.LL.AST as LL
import qualified Oat.LL.Name as LL
import Oat.X86.AST (Operand ((:$), (:$$), (:%)), (@@))
import qualified Oat.X86.AST as X86
import Prelude hiding (Const)

data BackendState = BackendState
  { _insStream :: X86Stream,
    _tyDecls :: HashMap LL.Name LL.Ty
  }

type BackendM = State BackendState

type MonadBackend = (MonadState BackendState)

data X86Elt
  = I X86.Ins
  | L !ByteString !Bool -- label, whether global or not

type X86Stream = [X86Elt]

L.makeFieldsNoPrefix ''BackendState

runBackend :: BackendM a -> BackendState -> a
runBackend = evalState

ins :: [X86.Ins] -> X86Stream
ins = fmap I

emit :: MonadBackend m => X86Stream -> m ()
emit ins = insStream %= (++> ins)

emitIns :: MonadBackend m => [X86.Ins] -> m ()
emitIns = emit . ins

emitMov :: MonadBackend m => X86.Operand -> X86.Operand -> m ()
emitMov src dst = case (src, dst) of
  (X86.Imm (X86.Lab _), _) -> error "immediate label not supported yet"
  (X86.Imm _, X86.Imm _) -> error "malformed mov"
  (X86.Imm _, _) -> mov
  (_, X86.Imm _) -> mov
  (X86.Reg _, _) -> mov
  (_, X86.Reg _) -> mov
  (_, _) -> emitIns [X86.Movq @@ [src, (:%) X86.Rax], X86.Movq @@ [(:%) X86.Rax, dst]]
  where
    mov = emitIns [X86.Movq @@ [src, dst]]

tySize' :: HashMap LL.Name LL.Ty -> LL.Ty -> Int
tySize' tyDecls =
  L.para go
  where
    go ty rs =
      case ty of
        LL.Void -> 0
        LL.I8 -> 0
        LL.I1 -> 8
        LL.I64 -> 8
        LL.TyFun _ -> 0
        LL.TyNamed name -> tySize' tyDecls (tyDecls ^?! L.ix name) -- only recurse here
        LL.TyPtr _ -> 8
        LL.TyArray n _ -> n * size
        LL.TyStruct _ -> size
      where
        size = sum rs

tySize :: MonadBackend m => LL.Ty -> m Int
tySize ty = do
  tyDecls <- gets _tyDecls
  pure $ tySize' tyDecls ty

compileOperand :: Alloc.Operand -> X86.Operand
compileOperand Null = (:$) 0
compileOperand (Const i) = (:$) i
compileOperand (Gid l) = (:$$) l
compileOperand (Loc loc) = compileLoc loc

compileLoc :: Alloc.Loc -> X86.Operand
compileLoc LVoid = error "compiling uid without location"
compileLoc (LReg r) = (:%) r
compileLoc (LStack i) = X86.Ind3 (X86.Lit (fromIntegral i * 8)) X86.Rbp
compileLoc (LLab l) = X86.Ind1 (X86.Lab l)

compileFunBody :: MonadBackend m => Alloc.FunBody -> m ()
compileFunBody (Alloc.FunBody insns) = forM_ insns $ \ins -> do
  case ins of
    Alloc.ILab (LLab l) -> emit [L l False]
    Alloc.ILab _ -> error "Malformed ILab"
    Alloc.PMov sms -> error "todo"
    Alloc.Icmp ins@Alloc.IcmpIns {_loc, _cmpOp, _arg1 = Loc (LReg r), _arg2} ->
      emitIns
        [ X86.Cmpq @@ [compileOperand _arg2, (:%) r],
          mapCmpOp _cmpOp @@ [compileOperand (Loc _loc)],
          X86.Andq @@ [(:$) 1, compileOperand (Loc _loc)]
        ]
    Alloc.Icmp Alloc.IcmpIns {_loc, _cmpOp, _arg1, _arg2} -> do
      emitMov (compileOperand _arg1) ((:%) X86.Rax)
      emitIns
        [ X86.Cmpq @@ [compileOperand _arg2, (:%) X86.Rax],
          mapCmpOp _cmpOp @@ [compileOperand (Loc _loc)],
          X86.Andq @@ [(:$) 1, compileOperand (Loc _loc)]
        ]
    Alloc.BinOp Alloc.BinOpIns {_loc = _loc@(LReg r), _op = LL.Add, _arg1, _arg2}
      | _arg2 == Loc _loc ->
        emitIns
          [ mapBinOp LL.Add @@ [compileOperand _arg1, compileOperand $ Loc _loc]
          ]
    Alloc.BinOp Alloc.BinOpIns {_loc = _loc@(LReg r), _op, _arg1, _arg2} ->
      emitIns
        []
    Alloc.Alloca Alloc.AllocaIns {_loc} -> undefined
    Alloc.Load li -> undefined
    Alloc.Store si -> undefined
    Alloc.Call ci -> undefined
    Alloc.Bitcast bi -> undefined
    Alloc.Gep gi -> undefined
    Alloc.Ret ri -> undefined
    Alloc.Br loc -> undefined
    Alloc.Cbr ci -> undefined
  where
    mapBinOp = \case
      LL.Add -> X86.Addq
      LL.Sub -> X86.Subq
      LL.Mul -> X86.Imulq
      LL.Shl -> X86.Shlq
      LL.Lshr -> X86.Shrq
      LL.Ashr -> X86.Sarq
      LL.And -> X86.Andq
      LL.Or -> X86.Orq
      LL.Xor -> X86.Xorq

    mapCmpOp = \case
      LL.Eq -> X86.Set X86.Eq
      LL.Neq -> X86.Set X86.Neq
      LL.Slt -> X86.Set X86.Lt
      LL.Sle -> X86.Set X86.Le
      LL.Sgt -> X86.Set X86.Gt
      LL.Sge -> X86.Set X86.Ge