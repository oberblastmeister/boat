{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.X86
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
    Mem (.., MemImm, MemLoc, MemStack, MemStackSimple),
    Loc,
    InstLab,
    X86,
    hasTwoOperands,
    dat,
    gText,
    text,
    memLocs,
    operandLocs,
    wordSize,
    callDests,
    callerSaves,
    calleeSaves,
    paramRegs,
    regs,
    instLabToElems,
  )
where

import Data.Int (Int64)
import Data.Strict.Wrapper (unstrict, pattern Strict)
import Oat.Asm qualified as Asm
import Prelude hiding (first, second)

data X86

instance Asm.Asm X86 where
  type Reg X86 = Reg
  type Mem X86 = Mem
  type Imm X86 = Imm
  type OpCode X86 = OpCode

type InstLab = Asm.InstLab X86

type Inst = Asm.Inst X86

type Loc = Asm.Loc X86

data Mem = Mem
  { displace :: Maybe Imm,
    first :: Maybe Loc,
    second :: Maybe Loc,
    scale :: Maybe Imm
  }
  deriving (Show, Eq)

pattern MemStackSimple :: Int64 -> Mem
pattern MemStackSimple displace = MemStack (Just (Lit displace)) Nothing Nothing

pattern MemStack :: Maybe Imm -> Maybe Loc -> Maybe Imm -> Mem
pattern MemStack displace second scale = Mem {displace, first = Just (Asm.LReg Rsp), second, scale}

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

operandLocs :: Traversal' Operand Loc
operandLocs = traversalVL $ \f -> \case
  Asm.Mem mem -> do
    mem <- traverseOf memLocs f mem
    pure $ Asm.Mem mem
  Asm.Loc loc -> Asm.Loc <$> f loc
  other -> pure other

memLocs :: Traversal' Mem Loc
memLocs = traversalVL $ \f mem@Mem {first, second} -> do
  first <- traverse f first
  second <- traverse f second
  pure $ mem {first, second}

hasTwoOperands :: OpCode -> Bool
hasTwoOperands = \case
  Movq -> True
  Leaq -> True
  Addq -> True
  Subq -> True
  Xorq -> True
  Orq -> True
  Andq -> True
  Shlq -> True
  Sarq -> True
  Shrq -> True
  Cmpq -> True
  _ -> False

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

type Operand = Asm.Operand X86

data Data
  = Asciz !ByteString
  | Quad !Imm
  deriving (Show, Eq)

data Asm
  = Text ![Inst]
  | Data ![Data]
  deriving (Show, Eq)

data Elem = Elem
  { lab :: !ByteString,
    global :: !Bool,
    asm :: !Asm
  }
  deriving (Show, Eq, Generic)

type Prog = [Elem]

makeFieldLabelsNoPrefix ''Mem
makeFieldLabelsNoPrefix ''Elem
makePrismLabels ''OpCode

-- instance Frame.IsFrame Frame where
--   newFrame = undefined
--   allocLocalWith = undefined
--   allocGlobal = undefined
--   framePointer = undefined
--   returnReg = undefined
--   prologue = prologue
--   _Call = #_Callq
--   _Move = #_Movq

--   | hasTwoOperands opcode,
--     [Asm.Temp name1, Asm.Temp name2] <- args = do
--       mem1 <- allocLocalM
--       mem2 <- allocLocalM
--       pure undefined
--       -- name1 <- f (mem1, False)
--       -- pure $ inst {Asm.args = [Asm.Temp name1, Asm.Mem mem2]}
--   | hasTwoOperands opcode, [_, _] <- args = pure inst
--   | hasTwoOperands opcode = error "Instruction should have two operands"
--   | Leaq <- opcode = error "todo"
--   | otherwise = pure inst

-- makeMove :: Operand -> Operand -> [Inst]
-- makeMove (Asm.Mem _) (Asm.Mem _) = error "Can't both be mem for moves"
-- makeMove arg1 arg2 = [Movq @ [arg1, arg2]]

-- needTemps :: Traversal Inst Inst (Mem, Bool) LL.Name
-- needTemps = traversalVL go
dat lab ds = Elem {lab, global = True, asm = Data ds}

--       | hasTwoOperands opcode,
text lab is = Elem {lab, global = False, asm = Text is}

gText :: ByteString -> [Inst] -> Elem
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

-- the number of bytes for a word
wordSize :: Int
wordSize = 8

instLabToElems :: [InstLab] -> Prog
instLabToElems instLabs =
  snd $
    unstrict $
      foldl'
        ( \(Strict (insts, prog)) -> \case
            Left (lab, global) -> Strict ([], Elem {lab, global, asm = Text insts} : prog)
            Right inst -> Strict (inst : insts, prog)
        )
        (Strict ([], []))
        instLabs
