{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86 where

import Data.ASCII (fromASCII)
import Data.Sequence qualified as Seq
import Oat.Asm.AST (pattern Move, pattern Op, pattern Reg, pattern Temp)
import Oat.Asm.AST qualified as Asm
import Oat.Frame qualified as Frame
import Oat.LL qualified as LL
import Oat.X86.AST (OpCode (..), Reg (..))
import Oat.X86.AST qualified as X86
import Optics
import Optics.Operators.Unsafe
import Optics.State.Operators
import Prelude hiding (Op)

pattern With :: LL.Inst -> LL.Operand
pattern With inst <- LL.Nested inst

data InstLifted
  = Inst X86.Inst
  | Lab ByteString

data BackendState = BackEndState
  { insts :: !(Seq InstLifted),
    frame :: !X86.Frame,
    tyDecls :: LL.TyMap
  }

type BackendM = State BackendState

type MonadBackend = MonadState BackendState

makeFieldLabelsNoPrefix ''BackendState

instance Frame.HasFrame BackendState X86.Frame where
  frameLens = #frame

tySize :: MonadBackend m => LL.Ty -> m Int
tySize ty = do
  tyDecls <- use #tyDecls
  pure $ LL.tySize tyDecls ty

lookupTy :: MonadBackend m => LL.Name -> m LL.Ty
lookupTy name = do
  mp <- use #tyDecls
  pure $ LL.lookupTy name mp

compileOperand :: LL.Operand -> X86.Operand
compileOperand (LL.Const i) = X86.ImmLit $ fromIntegral i
compileOperand (LL.Gid i) =
  Asm.Mem $
    X86.Mem
      { displace = Just $ X86.Lab $ fromASCII i,
        first = Just X86.Rip,
        second = Nothing,
        scale = Nothing
      }
compileOperand (LL.Temp t) = Asm.Temp t
-- invariant, it must have name
compileOperand (LL.Nested inst) = Asm.Temp $ inst ^?! LL.instName

emitInsts :: MonadBackend m => [X86.Inst] -> m ()
emitInsts insts = modify' (#insts %~ (<> Seq.fromList (Inst <$> insts)))

munchInst :: MonadBackend m => LL.Inst -> m ()
munchInst = \case
  LL.BinOp inst -> munchBinOp inst
  LL.Icmp inst -> munchIcmp inst
  -- LL.Alloca inst -> do
  --   mem <- Frame.allocLocalM
  --   allocLocalWithM
  other -> pure undefined

munchNested :: MonadBackend m => LL.Operand -> m ()
munchNested (LL.Nested inst) = munchInst inst
munchNested _ = pure ()

munchIcmp :: MonadBackend m => LL.IcmpInst -> m ()
munchIcmp LL.IcmpInst {name, op, arg1, arg2} = do
  emitInsts
    [ Move (compileOperand arg1) (Reg Rax),
      Op Cmpq [compileOperand arg2] [Reg Rax],
      Op Andq [X86.ImmLit 1] [Asm.Temp name]
    ]

munchBinOp :: MonadBackend m => LL.BinOpInst -> m ()
munchBinOp LL.BinOpInst {name, op, arg1, arg2}
  | LL.Mul <- op = do
      munchNested arg1
      munchNested arg2
      emitInsts
        [ Move (compileOperand arg2) (Reg Rax),
          Op Imulq [compileOperand arg1] [Reg Rax],
          Move (Reg Rax) (Temp name)
        ]
  | otherwise = do
      munchNested arg1
      munchNested arg2
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