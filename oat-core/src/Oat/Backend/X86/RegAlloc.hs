{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.RegAlloc
  ( spill,
    noReg,
    RegAllocMethod,
  )
where

import Control.Source qualified as Source
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Oat.Asm.AST (pattern (:@))
import Oat.Asm.AST qualified as Asm
import Oat.Backend.X86.Frame qualified as X86
import Oat.Backend.X86.Munch qualified as Munch
import Oat.Backend.X86.X86 (Reg (..))
import Oat.Backend.X86.X86 qualified as X86
import Oat.Backend.Frame qualified as Frame
import Oat.LL.Name qualified as LL

type RegAllocMethod =
  forall es.
  '[X86.Frame, LL.NameSource] :>> es =>
  Seq X86.InstLab ->
  Eff es (Seq X86.InstLab)

noReg :: RegAllocMethod
noReg insts = do
  let spills =
        -- TODO: List.nub is pretty bad I think, use some sort of ordered hashset, we are already converting to a hashmap anyway
        List.nub $
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
  -- we can do this because we are only using the stack, so it is impossible to have interferences
  -- therefore just set the extra temps to %r11
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
spill spills insts =
  act
    & execWriter @(Seq X86.InstLab)
    & runReader spills
  where
    act = for_ insts $ \case
      instLab@(Left _) -> tell $ fromList @(Seq _) [instLab]
      Right inst -> spillInst inst

spillInst :: SpillEffs :>> es => X86.Inst -> Eff es ()
spillInst inst@(opcode :@ args)
  | X86.hasTwoOperands opcode,
    [arg1, arg2] <- args = do
    arg1 <- spillOperand arg1
    arg2 <- spillOperand arg2
    case (arg1, arg2) of
      (Asm.Mem mem1, Asm.Mem mem2) -> do
        temp <- Source.fresh
        Munch.emitMov (Asm.Mem mem1) (Asm.Temp temp)
        Munch.emitInsts [inst {Asm.args = [Asm.Temp temp, Asm.Mem mem2]}]
      args -> Munch.emitInsts [inst {Asm.args = args ^.. both}]
  | X86.hasTwoOperands opcode = error "Instruction should have two operands"
  | otherwise = Munch.emitInst inst

spillOperand :: SpillEffs :>> es => X86.Operand -> Eff es X86.Operand
spillOperand arg@(Asm.Temp name) = do
  spills <- ask @SpillMap
  pure $ spills ^. at name <&> Asm.Mem & fromMaybe arg
spillOperand (Asm.Mem mem) = do
  spills <- ask @SpillMap
  mem <- forOf (X86.memLocs % #_LTemp) mem $ \name -> do
    case spills ^. at name of
      Just mem -> do
        name' <- Source.fresh
        Munch.emitMov (Asm.Mem mem) (Asm.Temp name')
        pure name'
      Nothing -> pure name
  pure $ Asm.Mem mem
spillOperand arg = pure arg
