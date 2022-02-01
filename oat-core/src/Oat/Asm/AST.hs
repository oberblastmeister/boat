{-# LANGUAGE TemplateHaskell #-}
-- the abstract assembly within a function body
{-# LANGUAGE UndecidableInstances #-}

module Oat.Asm.AST
  ( Reg,
    Mem,
    Imm,
    OpCode,
    Loc (..),
    Operand (.., Reg, Temp),
    Inst (.., Move, Op),
    OpIns (..),
    MoveIns (..),
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

data Inst a
  = InsOp (OpIns a)
  | InsLabel !LL.Name
  | InsMove (MoveIns a)

pattern Move :: Operand a -> Operand a -> Inst a
pattern Move src dst = InsMove MoveIns {src, dst}

pattern Op :: OpCode a -> [Operand a] -> Inst a
pattern Op op args = InsOp OpIns {op, args}

deriving instance AsmConstraint Show a => Show (Inst a)

deriving instance AsmConstraint Eq a => Eq (Inst a)

data OpIns a = OpIns {op :: !(OpCode a), args :: [Operand a]}

deriving instance AsmConstraint Show a => Show (OpIns a)

deriving instance AsmConstraint Eq a => Eq (OpIns a)

-- invariant, both must not be memory locations
data MoveIns a = MoveIns {src :: Operand a, dst :: Operand a}

deriving instance AsmConstraint Show a => Show (MoveIns a)

deriving instance AsmConstraint Eq a => Eq (MoveIns a)

$(makeFieldLabelsNoPrefix ''MoveIns)
$(makeFieldLabelsNoPrefix ''OpIns)