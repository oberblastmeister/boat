-- the abstract assembly within a function body
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Asm.AST
  ( Loc (..),
    Operand (.., Reg, Temp),
    Inst (.., (:@)),
    InstLab,
    instOperands,
    tempToReg,
  )
where

import Oat.Asm.Class (Asm (..), AsmConstraint)
import Oat.Common (swap)
import Oat.LL qualified as LL

type InstLab a = Either ByteString (Inst a)

data Loc a
  = LReg !(Reg a)
  | LTemp !LL.Name

instance (r ~ Reg a) => LabelOptic "_LReg" A_Prism (Loc a) (Loc a) r r where
  labelOptic = prism LReg (\case LReg reg -> Right reg; other -> Left other)

instance LabelOptic "_LTemp" A_Prism (Loc a) (Loc a) LL.Name LL.Name where
  labelOptic = prism LTemp (\case LTemp name -> Right name; other -> Left other)

deriving instance AsmConstraint Show a => Show (Loc a)

deriving instance AsmConstraint Eq a => Eq (Loc a)

tempToReg :: Setter (Loc a) (Loc a) LL.Name (Reg a)
tempToReg = swap #_LTemp #_LReg

data Operand a
  = Imm !(Imm a)
  | Mem !(Mem a)
  | Loc !(Loc a)

deriving instance AsmConstraint Show a => Show (Operand a)

deriving instance AsmConstraint Eq a => Eq (Operand a)

pattern Reg :: Asm a => Reg a -> Operand a
pattern Reg r = Loc (LReg r)

pattern Temp :: Asm a => LL.Name -> Operand a
pattern Temp t = Loc (LTemp t)

{-# COMPLETE Reg, Temp, Imm, Mem #-}

data Inst a = Inst {opcode :: !(OpCode a), args :: [Operand a]}

deriving instance AsmConstraint Show a => Show (Inst a)

deriving instance AsmConstraint Eq a => Eq (Inst a)

makeFieldLabelsNoPrefix ''Inst
makePrismLabels ''Operand

instOperands :: Traversal' (Inst a) (Operand a)
instOperands = #args % traversed

pattern (:@) :: Asm a => OpCode a -> [Operand a] -> Inst a
pattern opcode :@ args = Inst {opcode, args}

{-# COMPLETE (:@) #-}

infix 9 :@