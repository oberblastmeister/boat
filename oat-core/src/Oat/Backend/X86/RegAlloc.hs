{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.RegAlloc
  ( spill,
    noReg,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Source qualified as Source
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Oat.Asm.AST (pattern (:@))
import Oat.Asm.AST qualified as Asm
import Oat.Backend.X86.Codegen qualified as Codegen
import Oat.Frame qualified as Frame
import Oat.LL.Name qualified as LL
import Oat.X86.AST (Reg (..))
import Oat.X86.AST qualified as X86

noReg :: '[X86.Frame, LL.NameSource] :>> es => Seq X86.InstLab -> Eff es (Seq X86.InstLab)
noReg insts = do
  let spills =
        insts
          ^.. ( each % _Right
                  % Asm.instOperands
                  % X86.operandLocs
                  % #_LTemp
              )
  spillsMem <- for spills $ \spill -> do
    mem <- Frame.allocLocal
    pure (spill, mem)
  let spillsMap = HashMap.fromList spillsMem
  spilledInsts <- spill spillsMap insts
  let res =
        spilledInsts
          & ( each
                % _Right
                % Asm.instOperands
                % X86.operandLocs
                % Asm.tempToReg
            )
          %~ const R11
  pure res

type SpillEffs =
  '[ Writer (Seq X86.InstLab),
     Reader SpillMap,
     LL.NameSource
   ]

type SpillMap = HashMap LL.Name X86.Mem

spill :: '[LL.NameSource] :>> es => SpillMap -> Seq X86.InstLab -> Eff es (Seq X86.InstLab)
spill spills insts = do
  ((), insts') <- act'
  pure insts'
  where
    act' =
      act
        & runWriter @(Seq X86.InstLab)
        & runReader spills
    act = forOf_ (each % _Right) insts spillInst

spillInst :: SpillEffs :>> es => X86.Inst -> Eff es ()
spillInst inst@(opcode :@ args)
  | X86.hasTwoOperands opcode,
    [arg1, arg2] <- args = do
    arg1 <- spillOperand arg1
    arg2 <- spillOperand arg2
    case (arg1, arg2) of
      (Asm.Mem mem1, Asm.Mem mem2) -> do
        temp <- Source.fresh
        Codegen.emitMove (Asm.Mem mem1) (Asm.Temp temp)
        Codegen.emitInsts [inst {Asm.args = [Asm.Temp temp, Asm.Mem mem2]}]
      args -> Codegen.emitInsts [inst {Asm.args = args ^.. both}]
  | X86.hasTwoOperands opcode = error "Instruction should have two operands"
  | otherwise = Codegen.emitInst inst

spillOperand :: SpillEffs :>> es => X86.Operand -> Eff es X86.Operand
spillOperand arg@(Asm.Temp name) = do
  spills <- ask
  pure $ spills ^. at name <&> Asm.Mem & fromMaybe arg
spillOperand (Asm.Mem mem) = do
  spills <- ask
  mem <- forOf (X86.memLocs % #_LTemp) mem $ \name -> do
    case spills ^. at name of
      Just mem -> do
        name' <- Source.fresh
        Codegen.emitMove (Asm.Mem mem) (Asm.Temp name')
        pure name'
      Nothing -> pure name
  pure $ Asm.Mem mem
spillOperand arg = pure arg