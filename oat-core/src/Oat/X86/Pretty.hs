module Oat.X86.Pretty (prettyProg) where

import Oat.Asm.AST qualified as Asm
import Oat.X86.AST
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty

prettyProg :: Prog -> Doc ann
prettyProg elems = Pretty.vsep (prettyElem <$> elems)

prettyElem :: Elem -> Doc ann
prettyElem Elem {lab, global, asm} =
  sec <> glb <> Pretty.viaShow lab <> ":\n" <> body
  where
    glb =
      if global
        then "\t.global\t" <> Pretty.viaShow lab <> "\n"
        else ""
    (sec, body) = case asm of
      Text insts -> ("\t.text\n", Pretty.vsep (prettyInst <$> insts))
      Data datum -> ("\t.data\n", Pretty.vsep (prettyData <$> datum))

prettyData :: Data -> Doc ann
prettyData (Asciz bs) = "\t.asciz\t" <> Pretty.viaShow bs
prettyData (Quad imm) = "\t.quad\t" <> prettyImm imm

prettyReg :: Reg -> Doc ann
prettyReg = \case
  Rax -> "%rax"
  Rbx -> "%rbx"
  Rcx -> "%rcx"
  Rdx -> "%rdx"
  Rsi -> "%rsi"
  Rdi -> "%rdi"
  Rbp -> "%rbp"
  Rsp -> "%rsp"
  R8 -> "%r8"
  R9 -> "%r9"
  R10 -> "%r10"
  R11 -> "%r11"
  R12 -> "%r12"
  R13 -> "%r13"
  R14 -> "%r14"
  R15 -> "%r15"
  Rip -> "%rip"

prettyByteReg :: Reg -> Doc ann
prettyByteReg = \case
  Rax -> "%al"
  Rbx -> "%bl"
  Rcx -> "%cl"
  Rdx -> "%dl"
  Rsi -> "%sil"
  Rdi -> "%dil"
  Rbp -> "%bpl"
  Rsp -> "%spl"
  R8 -> "%r8b"
  R9 -> "%r9b"
  R10 -> "%r10b"
  R11 -> "%r11b"
  R12 -> "%r12b"
  R13 -> "%r13b"
  R14 -> "%r14b"
  R15 -> "%r15b"
  Rip -> error "%rip does not have a byte register"

prettyImm :: Imm -> Doc ann
prettyImm imm =
  "$" <> case imm of
    (Lit l) -> pretty l
    (Lab l) -> Pretty.viaShow l

prettyCond :: Cond -> Doc ann
prettyCond = \case
  Eq -> "eq"
  Neq -> "ne"
  Gt -> "g"
  Ge -> "ge"
  Lt -> "l"
  Le -> "le"

prettyOpCode :: OpCode -> Doc ann
prettyOpCode = \case
  Movq -> "movq"
  Pushq -> "pushq"
  Popq -> "popq"
  Leaq -> "leaq"
  Incq -> "incq"
  Decq -> "decq"
  Negq -> "negq"
  Notq -> "notq"
  Addq -> "addq"
  Subq -> "subq"
  Imulq -> "imulq"
  Xorq -> "xorq"
  Orq -> "ordq"
  Andq -> "andq"
  Shlq -> "shlq"
  Sarq -> "sarq"
  Shrq -> "shrq"
  Jmp -> "jmp"
  J c -> "j" <> prettyCond c
  Cmpq -> "cmpq"
  Set co -> "set" <> prettyCond co
  Callq -> "callq"
  Retq -> "retq"

prettyLocWith :: (Reg -> Doc ann) -> Loc -> Doc ann
prettyLocWith pReg (Asm.LReg r) = pReg r
prettyLocWith _ (Asm.LTemp t) = "t" <> Pretty.viaShow t

prettyLoc :: Loc -> Doc ann
prettyLoc = prettyLocWith prettyReg

prettyOperandWith :: (Reg -> Doc ann) -> Operand -> Doc ann
prettyOperandWith pReg = \case
  Asm.Imm i -> "$" <> prettyImm i
  Asm.Loc loc -> prettyLocWith pReg loc
  Asm.Mem mem -> prettyMem mem

prettyByteOperand :: Operand -> Doc ann
prettyByteOperand = prettyOperandWith prettyByteReg

prettyOperand :: Operand -> Doc ann
prettyOperand = prettyOperandWith prettyReg

prettyMem :: Mem -> Doc ann
prettyMem Mem {displace, first, second, scale} =
  maybe' prettyImm displace
    <> Pretty.parens
      ( maybe' prettyLoc first
          <> Pretty.comma
          <+> maybe' prettyLoc first
          <> maybe' prettyLoc second
          <> maybe' (\scale -> Pretty.comma <+> prettyImm scale) scale
      )
  where
    maybe' :: Monoid b => (a -> b) -> Maybe a -> b
    maybe' = maybe mempty

prettyJmpOperand :: Operand -> Doc ann
prettyJmpOperand = \case
  Asm.Imm imm -> prettyImm imm
  Asm.Loc loc -> "*" <> prettyLoc loc
  Asm.Mem mem -> "*" <> prettyMem mem

prettyInst :: Inst -> Doc ann
prettyInst Asm.Inst {opcode, args} = case opcode of
  Shlq -> handleShift
  Sarq -> handleShift
  Shrq -> handleShift
  _ -> "\t" <> prettyOpCode opcode <> "\t" <> Pretty.hsep (Pretty.punctuate Pretty.comma (go <$> args))
  where
    handleShift = prettyShift opcode args

    go = case opcode of
      J _ -> prettyJmpOperand
      Jmp -> prettyJmpOperand
      Callq -> prettyJmpOperand
      Set _ -> prettyByteOperand
      _ -> prettyOperand

-- instance Pretty Inst where
--   pretty Inst {opcode, operands} = case opcode of

prettyShift :: OpCode -> [Operand] -> Doc ann
prettyShift opcode operands = case operands of
  [Asm.Imm _, _] ->
    "\t" <> prettyOpCode opcode
      <> "\t"
      <> Pretty.hsep
        ( Pretty.punctuate
            Pretty.comma
            (prettyOperand <$> operands)
        )
  [Asm.Reg Rcx, dst] ->
    "\t" <> prettyOpCode opcode <> "\t%%cl" <> prettyOperand dst
  _ -> error "invalid shift operands"