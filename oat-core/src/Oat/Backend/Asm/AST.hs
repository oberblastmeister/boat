module Oat.Backend.Asm.AST where

class Asm a where
  type OpCode a = r | r -> a
  type Mem a = r | r -> a
  type Imm a = r | r -> a
  type Reg a = r | r -> a
  type Temp a = r | r -> a

data Inst a = Inst
  { opcode :: !(OpCode a),
    args :: [Operand a]
  }

data Operand a
  = OImm !(Imm a)
  | OMem !(Mem a)
  | OLoc !(Loc a)

data Loc a
  = LReg !(Reg a)
  | LTemp !(Temp a)

pattern (:@) :: OpCode a -> [Operand a] -> Inst a
pattern opcode :@ args = Inst {opcode, args}
