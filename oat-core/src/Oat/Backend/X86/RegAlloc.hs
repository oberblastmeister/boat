{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.RegAlloc where

import Data.DList (DList)
import Data.Source qualified as Source
import Oat.Asm.AST (pattern (:@))
import Oat.Asm.AST qualified as Asm
import Oat.Backend.X86.Codegen qualified as Codegen
import Oat.Frame qualified as Frame
import Oat.LL.Name qualified as LL
import Oat.X86.AST qualified as X86
import Optics.Operators.Unsafe ((^?!))

type SpillConstraint s m =
  ( MonadState s m,
    Frame.HasFrame s X86.Frame,
    LL.HasNameSource s,
    LabelOptic' "output" A_Lens s (DList X86.InstLab)
  )

data SpillState = SpillState
  { frame :: !(X86.Frame),
    source :: !(LL.NameSource),
    insts :: !(Seq X86.InstLab)
  }

type SpillMap = HashMap LL.Name X86.Mem

type MonadSpill = MonadState SpillState

makeFieldLabelsNoPrefix ''SpillState

-- noReg

-- spill :: SpillMap -> [X86.InstLab] -> Seq X86.InstLab
-- spill = 

spill' :: MonadSpill m => SpillMap -> [X86.InstLab] -> m ()
spill' spills insts = forOf_ (each % _Right) insts $ spillInst spills

spillInst :: MonadSpill m => SpillMap -> X86.Inst -> m ()
spillInst spills inst@(opcode :@ args)
  | X86.hasTwoOperands opcode,
    [arg1, arg2] <- args = do
      arg1 <- spillOperand spills arg1
      arg2 <- spillOperand spills arg2
      case (arg1, arg2) of
        (Asm.Mem mem1, Asm.Mem mem2) -> do
          temp <- Source.fresh
          Codegen.emitMove (Asm.Mem mem1) (Asm.Temp temp)
          Codegen.emitInsts [inst {Asm.args = [Asm.Temp temp, Asm.Mem mem2]}]
        args -> Codegen.emitInsts [inst {Asm.args = args ^.. both}]
  | X86.hasTwoOperands opcode = error "Instruction should have two operands"
  | otherwise = Codegen.emitInst inst

spillOperand :: MonadSpill m => SpillMap -> X86.Operand -> m X86.Operand
spillOperand spills arg@(Asm.Temp name) = pure $ spills ^. at name <&> Asm.Mem & fromMaybe arg
spillOperand spills (Asm.Mem mem) = do
  mem <- forOf (X86.memLocs % #_LTemp) mem $ \name -> do
    case spills ^. at name of
      Just mem -> do
        name' <- Source.fresh
        Codegen.emitMove (Asm.Mem mem) (Asm.Temp name')
        pure name'
      Nothing -> pure name
  pure $ Asm.Mem mem
spillOperand _ arg = pure arg