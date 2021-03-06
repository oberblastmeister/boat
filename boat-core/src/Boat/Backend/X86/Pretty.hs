module Boat.Backend.X86.Pretty (prettyProg) where

import Data.Text qualified as T
import Boat.Backend.X86.X86
import BoatPrelude.Unsafe qualified as Unsafe
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty

prettyProg :: Prog -> Doc ann
prettyProg elems = Pretty.vsep (prettyElem <$> elems)

prettyElem :: Elem -> Doc ann
prettyElem Elem {lab, global, asm} = do
  sec <> glb <> prettyLab lab <> ":\n" <> body
  where
    glb =
      if global
        then "\t.global\t" <> prettyLab lab <> "\n"
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

prettyLab :: ByteString -> Doc ann
prettyLab =
  pretty @Text
    . fst
    . Unsafe.fromJust
    . T.unsnoc
    . snd
    . Unsafe.fromJust
    . T.uncons
    . T.pack
    . show

prettyImm :: Imm -> Doc ann
prettyImm (Lit l) = pretty l
prettyImm (Lab l) = prettyLab l

prettyCond :: Cond -> Doc ann
prettyCond = \case
  Eq -> "e"
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
  Orq -> "orq"
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
prettyLocWith pReg (LReg r) = pReg r
prettyLocWith _ (LTemp t) = "t" <> Pretty.viaShow t

prettyLoc :: Loc -> Doc ann
prettyLoc = prettyLocWith prettyReg

prettyOperandWith :: (Reg -> Doc ann) -> Operand -> Doc ann
prettyOperandWith pReg = \case
  OImm i -> "$" <> prettyImm i
  OLoc loc -> prettyLocWith pReg loc
  OMem mem -> prettyMem mem

prettyByteOperand :: Operand -> Doc ann
prettyByteOperand = prettyOperandWith prettyByteReg

prettyOperand :: Operand -> Doc ann
prettyOperand = prettyOperandWith prettyReg

prettyMem :: Mem -> Doc ann
prettyMem Mem {offset, base, index, scale} = do
  maybe' prettyImm offset
    <> Pretty.parens
      ( maybe' prettyLoc base
          <> maybe' (\index -> Pretty.comma <+> prettyLoc index) index
          <> maybe' (\scale -> Pretty.comma <+> prettyScale scale) scale
      )
  where
    maybe' :: Monoid b => (a -> b) -> Maybe a -> b
    maybe' = maybe mempty

prettyScale :: Scale -> Doc ann
prettyScale S1 = "1"
prettyScale S2 = "2"
prettyScale S4 = "4"
prettyScale S8 = "8"

prettyJmpOperand :: Operand -> Doc ann
prettyJmpOperand = \case
  OImm imm -> prettyImm imm
  OLoc loc -> "*" <> prettyLoc loc
  OMem mem -> "*" <> prettyMem mem

prettyInst :: Inst -> Doc ann
prettyInst Inst {opcode, args} = case opcode of
  Shlq -> handleShift
  Sarq -> handleShift
  Shrq -> handleShift
  _ ->
    "\t"
      <> prettyOpCode opcode
      <> "\t"
      <> Pretty.hsep (Pretty.punctuate Pretty.comma (go <$> args))
  where
    handleShift = prettyShift opcode args

    go = case opcode of
      J _ -> prettyJmpOperand
      Jmp -> prettyJmpOperand
      Callq -> prettyJmpOperand
      Set _ -> prettyByteOperand
      _ -> prettyOperand

prettyShift :: OpCode -> [Operand] -> Doc ann
prettyShift opcode operands = case operands of
  [OImm _, _] ->
    "\t"
      <> prettyOpCode opcode
      <> "\t"
      <> Pretty.hsep
        ( Pretty.punctuate
            Pretty.comma
            (prettyOperand <$> operands)
        )
  [OReg Rcx, dst] ->
    "\t"
      <> prettyOpCode opcode
      <> "\t"
      <> prettyByteReg Rcx
      <> Pretty.comma
      <+> prettyOperand dst
  OReg _ : _ -> error "The first register for a shift operand must be %rcx"
  _ -> error "invalid shift operands"
