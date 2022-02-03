-- the abstract assembly within a function body
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Asm.AST
  ( Reg,
    Mem,
    Imm,
    OpCode,
    Loc (..),
    Operand (.., Reg, Temp),
    Inst (..),
    OpInst (..),
    MoveInst (..),
  )
where

import Oat.LL qualified as LL

type family Reg a

type family Mem a

type family Imm a

type family OpCode a

type AsmConstraint :: (Type -> Constraint) -> Type -> Constraint
type AsmConstraint c a = (c (Reg a), c (Mem a), c (Imm a), c (OpCode a))

data Loc a
  = LReg !(Reg a)
  | LTemp !LL.Name

deriving instance AsmConstraint Show a => Show (Loc a)

deriving instance AsmConstraint Eq a => Eq (Loc a)

data Operand a
  = Imm !(Imm a)
  | Mem !(Mem a)
  | Loc !(Loc a)

deriving instance AsmConstraint Show a => Show (Operand a)

deriving instance AsmConstraint Eq a => Eq (Operand a)

pattern Reg :: Reg a -> Operand a
pattern Reg r = Loc (LReg r)

pattern Temp :: LL.Name -> Operand a
pattern Temp t = Loc (LTemp t)

{-# COMPLETE Reg, Temp, Imm, Mem #-}

data Inst a = Inst {opcode :: !(OpCode a), args :: [Operand a]}

data Info a = Info
  { kind :: !InfoKind,
    src :: [Operand a],
    dst :: [Operand a]
  }

data InfoKind
  = IsCall
  | IsMove

deriving instance AsmConstraint Show a => Show (Inst a)

deriving instance AsmConstraint Eq a => Eq (Inst a)

data OpInst a = OpIns {op :: !(OpCode a), args :: [Operand a]}

deriving instance AsmConstraint Show a => Show (OpInst a)

deriving instance AsmConstraint Eq a => Eq (OpInst a)

-- invariant, both must not be memory locations
data MoveInst a = MoveIns {src :: Operand a, dst :: Operand a}

deriving instance AsmConstraint Show a => Show (MoveInst a)

deriving instance AsmConstraint Eq a => Eq (MoveInst a)

$(makeFieldLabelsNoPrefix ''MoveInst)
$(makeFieldLabelsNoPrefix ''OpInst)