{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.Munch
  ( emit,
    emitLabel,
    emitInst,
    emitInsts,
    emitMove,
    BackendEnv (..),
    compileBody,
  )
where

import Control.Source (Source)
import Control.Source qualified as Source
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Effectful.Reader.Static
import Effectful.Reader.Static.Optics
import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Effectful.Writer.Static.Local
import Oat.Asm.AST (pattern (:@))
import Oat.Asm.AST qualified as Asm
import Oat.Backend.X86.Frame as X86
import Oat.Backend.X86.X86 (InstLab, Reg (..))
import Oat.Backend.X86.X86 qualified as X86
import Oat.Common (concatToEither)
import Oat.Frame qualified as Frame
import Oat.LL qualified as LL
import Optics.Operators.Unsafe ((^?!))
import Prelude

pattern With :: LL.Inst -> LL.Operand
pattern With inst <- LL.Nested inst

data BackendEnv = BackendEnv
  { tyMap :: LL.TyMap
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

defBackendState :: BackendState
defBackendState = BackEndState {insts = mempty, allocaMems = mempty}

tySize :: Reader BackendEnv :> es => LL.Ty -> Eff es Int
tySize ty = do
  tyDecls <- rview #tyMap
  pure $ LL.tySize tyDecls ty

lookupTy :: Reader BackendEnv :> es => LL.Name -> Eff es LL.Ty
lookupTy name = do
  mp <- rview #tyMap
  pure $ LL.lookupTy name mp

compileOperand' :: BackendEffs :>> es => LL.Operand -> Eff es X86.Operand
compileOperand' (LL.Const i) = pure $ Asm.Imm $ X86.Lit $ fromIntegral i
compileOperand' (LL.Gid l) = pure $ Asm.Imm $ X86.Lab l
-- pure $
--   Asm.Mem $
--     X86.Mem
--       { displace = Just $ X86.Lab i,
--         first = Just $ Asm.LReg X86.Rip,
--         second = Nothing,
--         scale = Nothing
--       }
compileOperand' (LL.Temp t) = do
  mem <- use (#allocaMems % at t)
  pure $ case mem of
    Just mem -> Asm.Mem mem
    Nothing -> Asm.Temp t
-- invariant, it must have name
compileOperand' (LL.Nested inst) = pure $ Asm.Temp $ inst ^?! LL.instName

compileOperand :: BackendEffs :>> es => LL.Operand -> Eff es X86.Operand
compileOperand arg = munchNested arg *> compileOperand' arg

emit :: Writer (Seq InstLab) :> es => [InstLab] -> Eff es ()
emit = tell . Seq.fromList

emitLabel :: Writer (Seq InstLab) :> es => ByteString -> Eff es ()
emitLabel lab = emit [Left (lab, False)]

emitInst :: Writer (Seq InstLab) :> es => X86.Inst -> Eff es ()
emitInst inst = emit [Right inst]

emitInsts :: Writer (Seq InstLab) :> es => [X86.Inst] -> Eff es ()
emitInsts insts = emit (Right <$> insts)

-- | Performs some extra stuff to make sure the moves will be valid
-- In particular turns moves that have two memory locations with an intermediate temp in between
emitMove :: '[Writer (Seq InstLab), LL.NameSource] :>> es => X86.Operand -> X86.Operand -> Eff es ()
-- TODO: handle %rip relative global thingies
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

compileBody :: '[Reader BackendEnv, LL.NameSource, X86.Frame] :>> es => LL.FunBody -> Eff es (Seq InstLab)
compileBody = execWriter @(Seq _) . runState defBackendState . munchBody

munchBody :: BackendEffs :>> es => LL.FunBody -> Eff es ()
munchBody = traverseOf_ LL.bodyBlocks munchBlock

munchBlock :: BackendEffs :>> es => LL.Block -> Eff es ()
munchBlock block = do
  traverseOf_ (#insts % traversed) munchInst block
  munchTerm $ block ^. #term

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
  LL.Call inst -> compileCall inst
  LL.Bitcast LL.BitcastInst {name, arg} -> do
    arg <- compileOperand arg
    emitMove arg (Asm.Temp name)
  LL.Gep inst -> undefined

munchTerm :: BackendEffs :>> es => LL.Term -> Eff es ()
munchTerm = \case
  LL.Ret LL.RetTerm {arg = Just arg} -> do
    arg <- compileOperand arg
    emitMove arg (Asm.Reg Rax)
  LL.Ret LL.RetTerm {arg = Nothing} -> pure ()
  LL.Br name -> emitInsts [X86.Jmp :@ [Asm.Imm $ X86.Lab name]]
  LL.Cbr LL.CbrTerm {arg, lab1, lab2} -> do
    arg <- compileOperand arg
    emitInsts
      [ X86.Cmpq :@ [Asm.Imm $ X86.Lit 0, arg],
        X86.J X86.Neq :@ [Asm.Imm $ X86.Lab lab1],
        X86.Jmp :@ [Asm.Imm $ X86.Lab lab2]
      ]

compileCall :: BackendEffs :>> es => LL.CallInst -> Eff es ()
compileCall LL.CallInst {name, fn, args} = do
  fn <- compileOperand fn
  args <- traverse (compileOperand . snd) args
  viewShiftTo args
  emitInst $ X86.Callq :@ [fn]
  case name of
    Nothing -> pure ()
    Just name -> emitMove (Asm.Reg Rax) (Asm.Temp name)

viewShiftTo :: BackendEffs :>> es => [X86.Operand] -> Eff es ()
viewShiftTo args =
  concatToEither X86.paramRegs [i * 8 | i <- [0 :: Int64 ..]]
    & zip args
    & mapM_
      ( \case
          (arg, Left reg) ->
            emitMove arg (Asm.Reg reg)
          (arg, Right n) ->
            -- the top of the stack (where the stack pointer is) is the argument build up area
            -- we assume we have subtracted the stack to the largest call
            -- so these moves cannot interfere with locals stored on the stack
            emitMove arg (Asm.Mem $ X86.MemStackSimple n)
      )

munchNested :: BackendEffs :>> es => LL.Operand -> Eff es ()
munchNested (LL.Nested inst) = munchInst inst
munchNested _ = pure ()

munchIcmp :: BackendEffs :>> es => LL.IcmpInst -> Eff es ()
munchIcmp LL.IcmpInst {name, op, arg1, arg2} = do
  arg1 <- compileOperand arg1
  arg2 <- compileOperand arg2
  emitMove arg1 (Asm.Reg Rax)
  emitMove arg2 (Asm.Reg Rax)
  emitInsts [mapCmpOp op :@ [Asm.Temp name]]

munchBinOp :: BackendEffs :>> es => LL.BinOpInst -> Eff es ()
munchBinOp LL.BinOpInst {name, op, arg1, arg2}
  | LL.Mul <- op = do
    arg2 <- compileOperand arg2
    arg1 <- compileOperand arg1
    emitMove arg2 (Asm.Reg Rax)
    emitInsts [X86.Imulq :@ [arg1, Asm.Reg Rax]]
    emitMove (Asm.Reg Rax) (Asm.Temp name)
  | otherwise = do
    arg2 <- compileOperand arg2
    arg1 <- compileOperand arg1
    emitMove arg2 (Asm.Reg Rax)
    emitInsts [mapBinOp op :@ [arg1, Asm.Reg Rax]]
    emitMove (Asm.Reg Rax) (Asm.Temp name)

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
