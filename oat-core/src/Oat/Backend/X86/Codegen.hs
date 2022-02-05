{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module Oat.Backend.X86.Codegen
  ( munchBody,
    emit,
    emitLabel,
    emitInst,
    emitInsts,
    emitMove,
  )
where

import Data.Sequence qualified as Seq
import Data.Source (Source)
import Data.Source qualified as Source
import Effectful (Eff, type (:>), type (:>>))
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Effectful.Writer.Static.Local
import Oat.Asm.AST (pattern Reg, pattern Temp, pattern (:@))
import Oat.Asm.AST qualified as Asm
import Oat.Frame qualified as Frame
import Oat.LL qualified as LL
import Oat.X86.AST (InstLab, Reg (..))
import Oat.X86.AST qualified as X86
import Optics.Operators.Unsafe ((^?!))
import Prelude
import Effectful.Reader.Static.Optics

pattern With :: LL.Inst -> LL.Operand
pattern With inst <- LL.Nested inst

data BackendEnv = BackendEnv
  { tyDecls :: !LL.TyMap
  }

data BackendState = BackEndState
  { insts :: !(Seq InstLab),
    allocaMems :: !(HashMap LL.Name X86.Mem)
  }

makeFieldLabelsNoPrefix ''BackendState
makeFieldLabelsNoPrefix ''BackendEnv

type BackendEffs =
  '[ Writer (Seq InstLab),
     State BackendState,
     Reader BackendEnv,
     Source LL.Name,
     X86.Frame
   ]

tySize :: BackendEffs :>> es => LL.Ty -> Eff es Int
tySize ty = do
  tyDecls <- rview #tyDecls
  pure $ LL.tySize tyDecls ty

lookupTy :: BackendEffs :>> es => LL.Name -> Eff es LL.Ty
lookupTy name = do
  mp <- asks (^. #tyDecls)
  pure $ LL.lookupTy name mp

compileOperand' :: BackendEffs :>> es => LL.Operand -> Eff es X86.Operand
compileOperand' (LL.Const i) = pure $ Asm.Imm $ X86.Lit $ fromIntegral i
compileOperand' (LL.Gid i) =
  pure $
    Asm.Mem $
      X86.Mem
        { displace = Just $ X86.Lab i,
          first = Just $ Asm.LReg X86.Rip,
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

compileOperand :: BackendEffs :>> es => LL.Operand -> Eff es X86.Operand
compileOperand arg = munchNested arg >> compileOperand' arg

emit :: Writer (Seq InstLab) :> es => [InstLab] -> Eff es ()
emit = tell . Seq.fromList

emitLabel :: Writer (Seq InstLab) :> es => ByteString -> Eff es ()
emitLabel lab = emit [Left lab]

emitInst :: Writer (Seq InstLab) :> es => X86.Inst -> Eff es ()
emitInst inst = emit [Right inst]

emitInsts :: Writer (Seq InstLab) :> es => [X86.Inst] -> Eff es ()
emitInsts insts = emit (Right <$> insts)

emitMove :: '[Writer (Seq InstLab), LL.NameSource] :>> es => X86.Operand -> X86.Operand -> Eff es ()
emitMove arg1@(Asm.Mem _) arg2@(Asm.Mem _) = do
  temp <- Source.fresh
  emitInsts
    [ X86.Movq :@ [arg1, Asm.Temp temp],
      X86.Movq :@ [Asm.Temp temp, arg2]
    ]
emitMove arg1 arg2 = emitInsts [X86.Movq :@ [arg1, arg2]]

toMem :: X86.Operand -> X86.Operand
toMem (Asm.Loc loc) = Asm.Mem $ X86.MemLoc loc
toMem other = other

munchBody :: BackendEffs :>> es => LL.FunBody -> Eff es ()
munchBody = traverseOf_ LL.bodyInsts munchInst

munchInst :: BackendEffs :>> es => LL.Inst -> Eff es ()
munchInst = \case
  LL.BinOp inst -> munchBinOp inst
  LL.Icmp inst -> munchIcmp inst
  LL.Alloca inst -> do
    mem <- Frame.allocLocal
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

munchNested :: BackendEffs :>> es => LL.Operand -> Eff es ()
munchNested (LL.Nested inst) = munchInst inst
munchNested _ = pure ()

munchIcmp :: BackendEffs :>> es => LL.IcmpInst -> Eff es ()
munchIcmp LL.IcmpInst {name, op, arg1, arg2} = do
  arg1 <- compileOperand arg1
  arg2 <- compileOperand arg2
  emitInsts
    [ X86.Movq :@ [arg1, Reg Rax],
      X86.Cmpq :@ [arg2, Reg Rax],
      mapCmpOp op :@ [Asm.Temp name]
    ]

munchBinOp :: BackendEffs :>> es => LL.BinOpInst -> Eff es ()
munchBinOp LL.BinOpInst {name, op, arg1, arg2}
  | LL.Mul <- op = do
    arg2 <- compileOperand arg2
    arg1 <- compileOperand arg1
    emitInsts
      [ X86.Movq :@ [arg2, Reg Rax],
        X86.Imulq :@ [arg1, Reg Rax],
        X86.Movq :@ [Reg Rax, Temp name]
      ]
  | otherwise = do
    arg2 <- compileOperand arg2
    arg1 <- compileOperand arg1
    emitInsts
      [ X86.Movq :@ [arg2, Reg Rax],
        mapBinOp op :@ [arg1, Reg Rax],
        X86.Movq :@ [Reg Rax, Temp name]
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