{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.X86.AST
  ( Imm (..),
    Reg (..),
    Cond (..),
    OpCode (..),
    Inst,
    Data (..),
    Asm (..),
    Elem (..),
    Prog,
    Operand,
    Mem (.., MemImm, MemLoc),
    Frame (..),
    Loc,
    dat,
    gText,
    text,
  )
where

import Data.ASCII (ASCII)
import Data.Int (Int64)
import Oat.Asm.AST qualified as Asm
import Oat.Frame qualified as Frame

data Frame = Frame
  { stack :: !Int
  }

type Loc = Asm.Loc Frame

data Mem = Mem
  { displace :: Maybe Imm,
    first :: Maybe Loc,
    second :: Maybe Loc,
    scale :: Maybe Imm
  }
  deriving (Show, Eq)

pattern MemImm :: Imm -> Mem
pattern MemImm imm =
  Mem
    { displace = Just imm,
      first = Nothing,
      second = Nothing,
      scale = Nothing
    }

pattern MemLoc :: Loc -> Mem
pattern MemLoc loc =
  Mem
    { displace = Nothing,
      first = Just loc,
      second = Nothing,
      scale = Nothing
    }

instance Frame.Frame Frame where
  newFrame = undefined
  allocLocalWith = undefined
  allocGlobal = undefined
  framePointer = undefined
  returnReg = undefined
  prologue = undefined

type instance Asm.Reg Frame = Reg

type instance Asm.Mem Frame = Mem

type instance Asm.Imm Frame = Imm

type instance Asm.OpCode Frame = OpCode

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
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | Rip
  deriving (Show, Eq, Bounded, Enum)

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

type Inst = Asm.Inst Frame

type Operand = Asm.Operand Frame

data Data
  = Asciz !ByteString
  | Quad !Imm
  deriving (Show, Eq)

data Asm
  = Text ![Inst]
  | Data ![Data]
  deriving (Show, Eq)

data Elem = Elem
  { lab :: !(ASCII ByteString),
    global :: !Bool,
    asm :: !Asm
  }
  deriving (Show, Eq, Generic)

type Prog = [Elem]

makeFieldLabelsNoPrefix ''Frame
makeFieldLabelsNoPrefix ''Mem
makeFieldLabelsNoPrefix ''Elem

dat :: ASCII ByteString -> [Data] -> Elem
dat lab ds = Elem {lab, global = True, asm = Data ds}

text :: ASCII ByteString -> [Inst] -> Elem
text lab is = Elem {lab, global = False, asm = Text is}

gText :: ASCII ByteString -> [Inst] -> Elem
gText lab is = Elem {lab, global = True, asm = Text is}

regs :: [Reg]
regs = [minBound ..]

paramRegs :: [Reg]
paramRegs = [Rdi, Rsi, Rdx, Rcx, R8, R9]

calleeSaves :: [Reg]
calleeSaves = [Rbx, Rbp, R12, R13, R14, R15]

callerSaves :: [Reg]
callerSaves = [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11]

callDests :: [Reg]
callDests = [Rax, Rdx]
