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
import Oat.Backend.Frame qualified as Frame
import Oat.Backend.X86.Frame qualified as X86
import Oat.Backend.X86.Munch qualified as Munch
import Oat.Backend.X86.X86 (Reg (..), pattern (:@))
import Oat.Backend.X86.X86 qualified as X86
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
                    % #args
                    % traversed
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
                % #args
                % traversed
                % X86.operandLocs
                % X86.tempToReg
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
spillInst inst@(_ :@ args)
  | _ : _ : _ : _ <- args = error "Instructions should not have more than two operands"
  -- if there are two arguments, we need to make sure that both are not memory operands
  | [arg1, arg2] <- args = do
      arg1 <- spillOperand arg1
      arg2 <- spillOperand arg2
      case (arg1, arg2) of
        (X86.OMem mem1, X86.OMem mem2) -> do
          temp <- Source.fresh
          Munch.emitMov (X86.OMem mem1) (X86.OTemp temp)
          Munch.emitInsts [inst {X86.args = [X86.OTemp temp, X86.OMem mem2]}]
        args -> Munch.emitInsts [inst {X86.args = args ^.. both}]
  -- if there is less than two arguments, we can freely spill
  | otherwise = traverseOf (#args % each) spillOperand inst >>= Munch.emitInst

spillOperand :: SpillEffs :>> es => X86.Operand -> Eff es X86.Operand
spillOperand arg@(X86.OTemp name) = do
  spills <- ask @SpillMap
  pure $ spills ^. at name <&> X86.OMem & fromMaybe arg
spillOperand (X86.OMem mem) = do
  spills <- ask @SpillMap
  mem <- forOf (X86.memLocs % #_LTemp) mem $ \name -> do
    case spills ^. at name of
      Just mem -> do
        name' <- Source.fresh
        Munch.emitMov (X86.OMem mem) (X86.OTemp name')
        pure name'
      Nothing -> pure name
  pure $ X86.OMem mem
spillOperand arg = pure arg
