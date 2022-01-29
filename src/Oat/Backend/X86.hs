{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86 where

import qualified Data.Sequence as Seq
import Oat.Asm.AST (pattern Move, pattern Op, pattern Reg, pattern Temp)
import qualified Oat.Asm.AST as Asm
import qualified Oat.LL as LL
import Oat.X86.AST (OpCode (..), Reg (..))
import qualified Oat.X86.AST as X86
import Optics
import Optics.State.Operators
import Prelude hiding (Op)

pattern With :: LL.Inst -> LL.Operand
pattern With inst <- LL.Nested inst

data BackendState = BackendState
  { insts :: !(Seq X86.Inst)
  -- tyDecls :: HashMap (ASCII ByteString) LL.Ty
  }

type BackendM = State BackendState

type MonadBackend = MonadState BackendState

makeFieldLabelsNoPrefix ''BackendState

compileOperand :: LL.Operand -> X86.Operand
compileOperand (LL.Const i) = Asm.Const $ fromIntegral i
compileOperand (LL.Gid i) = undefined
-- compileOperand (LL.TempTree name _) = Asm.Loc $ Asm.LTemp name

emitInsts :: MonadBackend m => [X86.Inst] -> m ()
emitInsts insts = modify' (#insts %~ (<> Seq.fromList insts))

munchInst :: MonadBackend m => LL.Inst -> m ()
munchInst = \case
  LL.BinOp inst -> munchBinOp inst
  other -> pure undefined

munchBinOp :: MonadBackend m => LL.BinOpInst -> m ()
munchBinOp LL.BinOpInst {name, op, arg1, arg2}
  | LL.Mul <- op =
    emitInsts
      [ Move (compileOperand arg2) (Reg Rax),
        Op Imulq [compileOperand arg1] [Reg Rax],
        Move (Reg Rax) (Temp name)
      ]
  | otherwise =
    emitInsts
      [ Move (compileOperand arg2) (Reg Rax),
        Op (mapBinOp op) [compileOperand arg1] [Reg Rax],
        Move (Reg Rax) (Temp name)
      ]

mapBinOp :: LL.BinOp -> X86.OpCode
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

mapCmpOp :: LL.CmpOp -> X86.OpCode
mapCmpOp = \case
  LL.Eq -> X86.Set X86.Eq
  LL.Neq -> X86.Set X86.Neq
  LL.Slt -> X86.Set X86.Lt
  LL.Sle -> X86.Set X86.Le
  LL.Sgt -> X86.Set X86.Gt
  LL.Sge -> X86.Set X86.Ge