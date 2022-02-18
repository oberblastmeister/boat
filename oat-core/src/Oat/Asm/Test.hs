module Oat.Asm.Test where

data Inst opcode imm mem reg temp = Inst
  { opcode :: !opcode,
    args :: [Operand imm mem reg temp]
  }

data Operand imm mem reg temp
  = Imm !imm
  | Mem !mem
  | Loc !(Loc reg temp)

data Loc reg temp
  = LReg !reg
  | LTemp !temp

