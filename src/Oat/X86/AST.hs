{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.X86.AST
  ( Imm (..),
    Reg (..),
    Operand (.., (:%), (:$), (:$$)),
    Cond (..),
    OpCode (..),
    Ins (..),
    Data (..),
    Asm (..),
    Elem (..),
    Prog (..),
    (@@),
    operandHasLab,
    p1,
    dat,
    gText,
    argReg,
  )
where

import ASCII (ASCII)
import qualified Data.Text.Encoding as T
import Oat.Common (ascii)
import qualified Optics as O
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as P

data Imm
  = Lit !Int64
  | Lab !ByteString
  deriving (Show, Eq)

data Reg
  = Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | Rbp
  | Rsp
  | R08
  | R09
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | Rip
  deriving (Show, Eq, Bounded, Enum)

data Operand
  = Imm !Imm
  | Reg !Reg
  | Ind1 !Imm -- indirect: displacement
  | Ind2 !Reg -- indirect: (%reg)
  | Ind3 !Imm !Reg --indirect: displacement(%reg)
  deriving (Show, Eq)

operandHasLab :: Operand -> Bool
operandHasLab (Imm (Lab _)) = True
operandHasLab (Ind1 (Lab _)) = True
operandHasLab (Ind3 (Lab _) _) = True
operandHasLab _ = False

data Cond
  = Eq
  | Neq
  | Gt
  | Ge
  | Lt
  | Le
  deriving (Show, Eq)

data OpCode
  = Movq
  | Pushq
  | Popq
  | Leaq
  | Incq
  | Decq
  | Negq
  | Notq
  | Addq
  | Subq
  | Imulq
  | Xorq
  | Orq
  | Andq
  | Shlq
  | Sarq
  | Shrq
  | Jmp
  | J !Cond
  | Cmpq
  | Set !Cond
  | Callq
  | Retq
  deriving (Show, Eq)

data Ins = Ins
  { _opcode :: !OpCode,
    _operands :: ![Operand]
  }
  deriving (Show, Eq, Generic)

(@@) :: OpCode -> [Operand] -> Ins
(@@) _opcode _operands = Ins {_opcode, _operands}

data Data
  = Asciz !Text
  | Quad !Imm
  deriving (Show, Eq)

data Asm
  = Text ![Ins]
  | Data ![Data]
  deriving (Show, Eq)

data Elem = Elem
  { _lab :: !(ASCII ByteString),
    _global :: !Bool,
    _asm :: !Asm
  }
  deriving (Show, Eq, Generic)

newtype Prog = Prog [Elem]
  deriving (Show, Eq)

O.makeFieldLabelsNoPrefix ''Ins
O.makeFieldLabelsNoPrefix ''Elem

instance Pretty Reg where
  pretty = \case
    Rax -> "%rax"
    Rbx -> "%rbx"
    Rcx -> "%rcx"
    Rdx -> "%rdx"
    Rsi -> "%rsi"
    Rdi -> "%rdi"
    Rbp -> "%rbp"
    Rsp -> "%rsp"
    R08 -> "%r8"
    R09 -> "%r9"
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
  R08 -> "%r8b"
  R09 -> "%r9b"
  R10 -> "%r10b"
  R11 -> "%r11b"
  R12 -> "%r12b"
  R13 -> "%r13b"
  R14 -> "%r14b"
  R15 -> "%r15b"
  Rip -> error "Cannot use %rip as byte register"

instance Pretty Imm where
  pretty (Lit l) = pretty l
  -- maybe fix this to not use show
  pretty (Lab l) = pretty $ show @Text l

prettyCond :: Cond -> Doc ann
prettyCond = \case
  Eq -> "eq"
  Neq -> "ne"
  Gt -> "g"
  Ge -> "ge"
  Lt -> "l"
  Le -> "le"

instance Pretty OpCode where
  pretty = \case
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

prettyOperand' :: (Reg -> Doc ann) -> Operand -> Doc ann
prettyOperand' prettyReg = \case
  Imm i -> "$" <> pretty i
  Reg r -> prettyReg r
  Ind1 i -> pretty i
  Ind2 r -> P.parens $ pretty r
  Ind3 i r -> pretty i <> P.parens (pretty r)

prettyJmpOperand :: Operand -> Doc ann
prettyJmpOperand = \case
  Imm i -> pretty i
  Reg r -> "*" <> pretty r
  Ind1 i -> "*" <> pretty i
  Ind2 r -> "*" <> P.parens (pretty r)
  Ind3 i r -> "*" <> pretty i <> P.parens (pretty r)

prettyByteOperand :: Operand -> Doc ann
prettyByteOperand = prettyOperand' prettyByteReg

prettyOperand :: Operand -> Doc ann
prettyOperand = prettyOperand' pretty

instance Pretty Ins where
  pretty Ins {_opcode, _operands} = case _opcode of
    Shlq -> handleShift
    Sarq -> handleShift
    Shrq -> handleShift
    _ -> "\t" <> pretty _opcode <> "\t" <> P.hsep (P.punctuate P.comma (f <$> _operands))
    where
      handleShift = prettyShift _opcode _operands

      f = case _opcode of
        J _ -> prettyJmpOperand
        Jmp -> prettyJmpOperand
        Callq -> prettyJmpOperand
        Set _ -> prettyByteOperand
        _ -> prettyOperand

prettyShift :: OpCode -> [Operand] -> Doc ann
prettyShift opcode operands = case operands of
  [Imm _i, _dst] -> "\t" <> pretty opcode <> "\t" <> P.hsep (P.punctuate P.comma (prettyOperand <$> operands))
  [Reg Rcx, dst] ->
    "\t" <> pretty opcode <> "\t%%cl" <> prettyOperand dst
  _ -> error "Invalid operands"

instance Pretty Data where
  -- show for ByteString includes the quotes around it
  pretty (Asciz t) = "\t.asciz\t" <> P.viaShow (T.encodeUtf8 t)
  pretty (Quad i) = "\t.quad\t" <> pretty i

instance Pretty Asm where
  pretty (Text is) = "\t.text\n" <> P.vsep (pretty <$> is)
  pretty (Data ds) = "\t.data\n" <> P.vsep (pretty <$> ds)

instance Pretty Elem where
  pretty Elem {_lab, _global, _asm} =
    sec <> glb <> P.viaShow _lab <> ":\n" <> body
    where
      glb =
        if _global
          then "\t.global\t" <> P.viaShow _lab <> "\n"
          else ""
      (sec, body) = case _asm of
        Text is -> ("\t.text\n", P.vsep (pretty <$> is))
        Data ds -> ("\t.data\n", P.vsep (pretty <$> ds))

instance Pretty Prog where
  pretty (Prog elems) = P.vsep (pretty <$> elems)

dat :: ASCII ByteString -> [Data] -> Elem
dat _lab ds = Elem {_lab, _global = True, _asm = Data ds}

text :: ASCII ByteString -> [Ins] -> Elem
text _lab is = Elem {_lab, _global = False, _asm = Text is}

gText :: ASCII ByteString -> [Ins] -> Elem
gText _lab is = Elem {_lab, _global = True, _asm = Text is}

pattern (:%) :: Reg -> Operand
pattern (:%) r = Reg r

{-# COMPLETE (:%) #-}

pattern (:$) :: Int64 -> Operand
pattern (:$) i = Imm (Lit i)

{-# COMPLETE (:$) #-}

pattern (:$$) :: ByteString -> Operand
pattern (:$$) l = Imm (Lab l)

{-# COMPLETE (:$$) #-}

p1 :: Prog
p1 =
  Prog
    [ text
        (ascii "foo")
        [ Ins Xorq [(:%) Rax, (:%) Rax],
          Ins Movq [(:$) 100, Reg Rax],
          Ins Retq []
        ]
    ]

argReg :: Int -> Maybe Reg
argReg = \case
  0 -> Just Rdi
  1 -> Just Rsi
  2 -> Just Rdx
  3 -> Just Rcx
  4 -> Just R08
  5 -> Just R09
  _ -> Nothing