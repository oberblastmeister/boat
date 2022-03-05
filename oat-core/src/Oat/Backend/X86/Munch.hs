{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.Munch
  ( emit,
    emitLabel,
    emitInst,
    emitInsts,
    emitMov,
    compileBody,
  )
where

import Acc (Acc)
import Data.Int (Int64)
import Data.Vector qualified as V
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Effectful.Writer.Static.Local
import Oat.Backend.Frame qualified as Frame
import Oat.Backend.X86.Frame qualified as X86.Frame
import Oat.Backend.X86.X86 (InstLab, Reg (..), pattern (:@))
import Oat.Backend.X86.X86 qualified as X86
import Oat.LL qualified as LL
import Oat.Utils.Misc (concatToEither)
import Oat.Utils.Source (Source)
import Oat.Utils.Source qualified as Source
import Optics.Operators.Unsafe ((^?!))
import Prelude

data BackendState = BackEndState
  { insts :: !(Acc InstLab),
    allocaMems :: !(HashMap LL.Name X86.Mem)
  }

makeFieldLabelsNoPrefix ''BackendState

type BackendEffs =
  '[ Writer (Acc InstLab),
     State BackendState,
     Reader LL.DeclMap,
     Source LL.Name,
     X86.Frame.Frame
   ]

defBackendState :: BackendState
defBackendState = BackEndState {insts = mempty, allocaMems = mempty}

compileOperand' :: BackendEffs :>> es => LL.Operand -> Eff es X86.Operand
compileOperand' (LL.Const i) = pure $ X86.OImm $ X86.Lit $ fromIntegral i
compileOperand' (LL.Gid l) = pure $ X86.OImm $ X86.Lab l
compileOperand' (LL.Temp t) = do
  mem <- use @BackendState $ #allocaMems % at t
  pure $ case mem of
    Just mem -> X86.OMem mem
    Nothing -> X86.OTemp t
-- invariant, it must have name
compileOperand' (LL.Nested inst) = pure $ X86.OTemp $ inst ^?! LL.instName

compileOperand :: BackendEffs :>> es => LL.Operand -> Eff es X86.Operand
compileOperand arg = munchNested arg *> compileOperand' arg

emit :: Writer (Acc InstLab) :> es => [InstLab] -> Eff es ()
emit = tell @(Acc _) . fromList

emitLabel :: Writer (Acc InstLab) :> es => ByteString -> Eff es ()
emitLabel lab = emit [Left (lab, False)]

emitInst :: Writer (Acc InstLab) :> es => X86.Inst -> Eff es ()
emitInst inst = emit [Right inst]

emitInsts :: Writer (Acc InstLab) :> es => [X86.Inst] -> Eff es ()
emitInsts insts = emit (Right <$> insts)

-- | Performs some extra stuff to make sure the moves will be valid
-- In particular turns moves that have two memory locations with an intermediate temp in between
emitMov :: '[Writer (Acc InstLab), LL.NameSource] :>> es => X86.Operand -> X86.Operand -> Eff es ()
-- TODO: handle %rip relative global thingies
emitMov arg1@(X86.OMem _) arg2@(X86.OMem _) = do
  temp <- Source.fresh
  emitInsts
    [ X86.Movq :@ [arg1, X86.OTemp temp],
      X86.Movq :@ [X86.OTemp temp, arg2]
    ]
emitMov arg1 arg2 = emitInsts [X86.Movq :@ [arg1, arg2]]

toMem :: X86.Operand -> X86.Operand
toMem (X86.OLoc loc) = X86.OMem $ X86.MemLoc loc
toMem other = other

compileBody :: '[Reader LL.DeclMap, LL.NameSource, X86.Frame.Frame] :>> es => LL.FunBody -> Eff es (Acc InstLab)
compileBody = execWriter @(Acc _) . runState defBackendState . munchBody

munchBody :: BackendEffs :>> es => LL.FunBody -> Eff es ()
munchBody body = do
  munchBlock $ body ^. #entry
  traverse_ munchLabBlock (body ^. #labeled)

munchBlock :: BackendEffs :>> es => LL.Block -> Eff es ()
munchBlock block = do
  traverseOf_ (#insts % traversed) munchInst block
  munchTerm $ block ^. #term

munchLabBlock :: BackendEffs :>> es => LL.LabBlock -> Eff es ()
munchLabBlock labBlock = do
  emitLabel $ labBlock ^. #lab
  munchBlock $ labBlock ^. #block

munchInst :: BackendEffs :>> es => LL.Inst -> Eff es ()
munchInst = \case
  LL.BinOp inst -> munchBinOp inst
  LL.Icmp inst -> munchIcmp inst
  LL.Alloca inst -> do
    mem <- Frame.allocLocal
    assign @BackendState (#allocaMems % at (inst ^. #name)) (Just mem)
  LL.Load LL.LoadInst {name, arg} -> do
    arg <- compileOperand arg
    emitMov arg (X86.OTemp name)
  LL.Store LL.StoreInst {arg1, arg2} -> do
    arg1 <- compileOperand arg1
    arg2 <- compileOperand arg2
    emitMov arg1 (toMem arg2)
  LL.Call inst -> compileCall inst
  LL.Bitcast LL.BitcastInst {name, arg} -> do
    arg <- compileOperand arg
    emitMov arg (X86.OTemp name)
  LL.Gep _ -> error "gep should be lowered before this"
  LL.Select inst -> undefined
  LL.Ext LL.ExtInst {op = LL.Zext, name, arg} -> do
    arg <- compileOperand arg
    -- everything is already represented using 64 bytes
    -- so zero extension just doesn't do anything
    emitMov arg (X86.OTemp name)
  LL.Ext LL.ExtInst {} -> error "other ext instructions not support yet"

munchTerm :: BackendEffs :>> es => LL.Term -> Eff es ()
munchTerm = \case
  LL.Ret LL.RetTerm {arg = Just arg} -> do
    arg <- compileOperand arg
    emitMov arg (X86.OReg Rax)
  LL.Ret LL.RetTerm {arg = Nothing} -> pure ()
  LL.Br name -> emitInsts [X86.Jmp :@ [X86.OImm $ X86.Lab name]]
  LL.Cbr LL.CbrTerm {arg, lab1, lab2} -> do
    arg <- compileOperand arg
    emitInsts
      [ X86.Cmpq :@ [X86.OImm $ X86.Lit 0, arg],
        X86.J X86.Neq :@ [X86.OImm $ X86.Lab lab1],
        X86.Jmp :@ [X86.OImm $ X86.Lab lab2]
      ]

compileCall :: BackendEffs :>> es => LL.CallInst -> Eff es ()
compileCall LL.CallInst {name, fn, args} = do
  fn <- compileOperand fn
  args <- traverse (compileOperand . snd) args
  viewShiftTo args
  emitInst $ X86.Callq :@ [fn]
  case name of
    Nothing -> pure ()
    Just name -> emitMov (X86.OReg Rax) (X86.OTemp name)

viewShiftTo :: BackendEffs :>> es => Vec X86.Operand -> Eff es ()
viewShiftTo args =
  fromList (concatToEither X86.paramRegs [i * 8 | i <- [0 :: Int64 ..]])
    & V.zip args
    & mapM_
      ( \case
          (arg, Left reg) ->
            emitMov arg (X86.OReg reg)
          (arg, Right n) ->
            -- the top of the stack (where the stack pointer is) is the argument build up area
            -- we assume we have subtracted the stack to the largest call
            -- so these moves cannot interfere with locals stored on the stack
            emitMov arg (X86.OMem $ X86.MemStackSimple n)
      )

munchNested :: BackendEffs :>> es => LL.Operand -> Eff es ()
munchNested (LL.Nested inst) = munchInst inst
munchNested _ = pure ()

munchIcmp :: BackendEffs :>> es => LL.IcmpInst -> Eff es ()
munchIcmp LL.IcmpInst {name, op, arg1, arg2} = do
  arg1 <- compileOperand arg1
  arg2 <- compileOperand arg2
  emitMov arg1 (X86.OReg Rax)
  emitInsts
    [ X86.Cmpq :@ [arg2, X86.OReg Rax],
      -- set only works on byte registers for now
      mapCmpOp op :@ [X86.OTemp name],
      -- only keep the lower order byte because set only sets the byte register
      X86.Andq :@ [X86.OImm $ X86.Lit 1, X86.OTemp name]
    ]

munchBinOp :: BackendEffs :>> es => LL.BinOpInst -> Eff es ()
munchBinOp LL.BinOpInst {name, op, arg1, arg2}
  | LL.isShiftOp op = do
      arg2 <- compileOperand arg2
      arg1 <- compileOperand arg1
      -- this is actually a special case
      -- shift operations actually use %cl which is the byte register for %rcx
      -- we just use %rcx to simplify the handling of this
      -- during prettying we will make sure to use %cl
      emitMov arg2 (X86.OReg Rcx)
      emitInsts [mapBinOp op :@ [X86.OReg Rcx, arg1]]
      emitMov arg1 (X86.OTemp name)
  | LL.Mul <- op = do
      arg2 <- compileOperand arg2
      arg1 <- compileOperand arg1
      emitMov arg2 (X86.OReg Rax)
      -- imulq must use %rax
      emitInsts [X86.Imulq :@ [arg1, X86.OReg Rax]]
      emitMov (X86.OReg Rax) (X86.OTemp name)
  | otherwise = do
      arg2 <- compileOperand arg2
      arg1 <- compileOperand arg1
      emitMov arg2 (X86.OReg Rax)
      emitInsts [mapBinOp op :@ [arg1, X86.OReg Rax]]
      emitMov (X86.OReg Rax) (X86.OTemp name)

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
