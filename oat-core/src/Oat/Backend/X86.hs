{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86
  ( munchInst,
  )
where

import Data.ASCII (fromASCII)
import Data.IdGen (IdGen)
import Data.IdGen qualified as IdGen
import Data.Sequence qualified as Seq
import Oat.Asm.AST (pattern Move, pattern Op, pattern Reg, pattern Temp)
import Oat.Asm.AST qualified as Asm
import Oat.Frame qualified as Frame
import Oat.LL qualified as LL
import Oat.X86.AST (OpCode (..), Reg (..))
import Oat.X86.AST qualified as X86
import Optics
import Optics.Operators.Unsafe
import Prelude

pattern With :: LL.Inst -> LL.Operand
pattern With inst <- LL.Nested inst

data InstLifted
  = Inst X86.Inst
  | Lab ByteString

data BackendState = BackEndState
  { insts :: !(Seq InstLifted),
    frame :: !X86.Frame,
    tyDecls :: !(LL.TyMap),
    allocaMems :: !(HashMap LL.Name X86.Mem),
    idGen :: !IdGen
  }

type BackendM = State BackendState

type MonadBackend = MonadState BackendState

makeFieldLabelsNoPrefix ''BackendState

instance Frame.HasFrame BackendState X86.Frame where
  frame = #frame

instance IdGen.HasIdGen BackendState IdGen where
  idGen = #idGen

tySize :: MonadBackend m => LL.Ty -> m Int
tySize ty = do
  tyDecls <- use #tyDecls
  pure $ LL.tySize tyDecls ty

lookupTy :: MonadBackend m => LL.Name -> m LL.Ty
lookupTy name = do
  mp <- use #tyDecls
  pure $ LL.lookupTy name mp

compileOperand' :: MonadBackend m => LL.Operand -> m X86.Operand
compileOperand' (LL.Const i) = pure $ Asm.Imm $ X86.Lit $ fromIntegral i
compileOperand' (LL.Gid i) =
  pure $
    Asm.Mem $
      X86.Mem
        { displace = Just $ X86.Lab $ fromASCII i,
          first = Just $ Asm.LReg $ X86.Rip,
          second = Nothing,
          scale = Nothing
        }
compileOperand' (LL.Temp t) = do
  mem <- use (#allocaMems % at t)
  pure $ case mem of
    Just mem -> Asm.Mem mem
    Nothing -> Asm.Temp t
-- invariant, it must have name
compileOperand' (LL.Nested inst) = pure $ Asm.Temp $ inst ^?! LL.instName

compileOperand :: MonadBackend m => LL.Operand -> m X86.Operand
compileOperand arg = munchNested arg >> compileOperand' arg

emit :: MonadBackend m => [InstLifted] -> m ()
emit insts = modify' (#insts %~ (<> Seq.fromList insts))

emitLabel :: MonadBackend m => ByteString -> m ()
emitLabel lab = emit [Lab lab]

emitInsts :: MonadBackend m => [X86.Inst] -> m ()
emitInsts insts = emit (Inst <$> insts)

emitMove :: MonadBackend m => X86.Operand -> X86.Operand -> m ()
emitMove arg1@(Asm.Mem _) arg2@(Asm.Mem _) = do
  temp <- IdGen.freshTemp
  emitInsts
    [ Asm.Move arg1 (Asm.Temp temp),
      Asm.Move (Asm.Temp temp) arg2
    ]
emitMove arg1 arg2 = emitInsts [Asm.Move arg1 arg2]

toMem :: X86.Operand -> X86.Operand
toMem (Asm.Loc loc) = Asm.Mem $ X86.MemLoc loc
toMem other = other

munchBody :: MonadBackend m => LL.FunBody -> m ()
munchBody = traverseOf_ LL.bodyInsts munchInst

munchInst :: MonadBackend m => LL.Inst -> m ()
munchInst = \case
  LL.BinOp inst -> munchBinOp inst
  LL.Icmp inst -> munchIcmp inst
  LL.Alloca inst -> do
    mem <- Frame.allocLocalM
    #allocaMems % at (inst ^. #name) ?= mem
  LL.Load LL.LoadInst {name, arg} -> do
    arg <- compileOperand arg
    emitMove arg (Asm.Temp name)
  LL.Store LL.StoreInst {arg1, arg2} -> do
    arg1 <- compileOperand arg1
    arg2 <- compileOperand arg2
    emitMove arg2 (toMem arg1)
  LL.Call inst -> undefined
  LL.Bitcast LL.BitcastInst {name, arg} -> do
    arg <- compileOperand arg
    emitMove arg (Asm.Temp name)
  LL.Gep inst -> undefined

munchNested :: MonadBackend m => LL.Operand -> m ()
munchNested (LL.Nested inst) = munchInst inst
munchNested _ = pure ()

munchIcmp :: MonadBackend m => LL.IcmpInst -> m ()
munchIcmp LL.IcmpInst {name, op, arg1, arg2} = do
  arg1 <- compileOperand arg1
  arg2 <- compileOperand arg2
  emitInsts
    [ Move arg1 (Reg Rax),
      Op Cmpq [arg2, Reg Rax],
      Op (mapCmpOp op) [Asm.Temp name]
    ]

munchBinOp :: MonadBackend m => LL.BinOpInst -> m ()
munchBinOp LL.BinOpInst {name, op, arg1, arg2}
  | LL.Mul <- op = do
      arg2 <- compileOperand arg2
      arg1 <- compileOperand arg1
      emitInsts
        [ Move (arg2) (Reg Rax),
          Op Imulq [arg1, Reg Rax],
          Move (Reg Rax) (Temp name)
        ]
  | otherwise = do
      arg2 <- compileOperand arg2
      arg1 <- compileOperand arg1
      emitInsts
        [ Move (arg2) (Reg Rax),
          Op (mapBinOp op) [arg1, Reg Rax],
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