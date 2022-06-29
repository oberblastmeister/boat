{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Boat.Backend.X86.X86
  ( Imm (..),
    Reg (..),
    Cond (..),
    OpCode (..),
    Data (..),
    Asm (..),
    Elem (..),
    Prog,
    Scale (..),
    InstLab,
    Inst (.., (:@)),
    Operand (.., OReg, OTemp),
    Loc (..),
    Mem (.., MemImm, MemLoc, MemStack, MemStackSimple, MemBaseSimple),
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
    tempToReg,
  )
where

import Data.Int (Int64)
import Boat.LL qualified as LL
import Boat.Utils.Optics (swap)

type InstLab = Either (LL.Name, Bool) Inst

data Inst = Inst
  { opcode :: !OpCode,
    args :: [Operand]
  }
  deriving (Show, Eq)

pattern (:@) :: OpCode -> [Operand] -> Inst
pattern opcode :@ args = Inst {opcode, args}

{-# COMPLETE (:@) #-}

data Operand
  = OImm !Imm
  | OMem !Mem
  | OLoc !Loc
  deriving (Show, Eq)

data Loc
  = LReg !Reg
  | LTemp !LL.Name
  deriving (Show, Eq)

-- M[offset + R[base] + R[index] * scale]
data Mem = Mem
  { offset :: Maybe Imm,
    base :: Maybe Loc,
    index :: Maybe Loc,
    scale :: Maybe Scale
  }
  deriving (Show, Eq)

data Scale
  = S1
  | S2
  | S4
  | S8
  deriving (Show, Eq)

pattern OReg :: Reg -> Operand
pattern OReg reg = OLoc (LReg reg)

pattern OTemp :: LL.Name -> Operand
pattern OTemp temp = OLoc (LTemp temp)

pattern MemBaseSimple :: Int64 -> Mem
pattern MemBaseSimple offset =
  Mem
    { offset = Just (Lit offset),
      base = Just (LReg Rbp),
      index = Nothing,
      scale = Nothing
    }

pattern MemStackSimple :: Int64 -> Mem
pattern MemStackSimple offset = MemStack (Just (Lit offset)) Nothing Nothing

pattern MemStack :: Maybe Imm -> Maybe Loc -> Maybe Scale -> Mem
pattern MemStack offset index scale =
  Mem
    { offset,
      base = Just (LReg Rsp),
      index,
      scale
    }

pattern MemImm :: Imm -> Mem
pattern MemImm imm =
  Mem
    { offset = Just imm,
      base = Nothing,
      index = Nothing,
      scale = Nothing
    }

pattern MemLoc :: Loc -> Mem
pattern MemLoc loc =
  Mem
    { offset = Nothing,
      base = Just loc,
      index = Nothing,
      scale = Nothing
    }

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

$(makeFieldLabelsNoPrefix ''Inst)
$(makeFieldLabelsNoPrefix ''Mem)
$(makeFieldLabelsNoPrefix ''Elem)
$(makePrismLabels ''Operand)
$(makePrismLabels ''Loc)
$(makePrismLabels ''Scale)

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
dat :: ByteString -> [Data] -> Elem
dat lab ds = Elem {lab, global = True, asm = Data ds}

--       | hasTwoOperands opcode,
text :: ByteString -> [Inst] -> Elem
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
    foldr
      ( \instLab (insts, mod) -> case instLab of
          Left (lab, global) -> ([], Elem {lab, global, asm = Text insts} : mod)
          Right inst -> (inst : insts, mod)
      )
      ([], [])
      instLabs

-- instOperands :: Traversal' Inst Operand
-- instOperands = undefined

tempToReg :: Setter Loc Loc LL.Name Reg
tempToReg = swap #_LTemp #_LReg

operandLocs :: Traversal' Operand Loc
operandLocs = traversalVL $ \f -> \case
  OMem mem -> do
    mem <- traverseOf memLocs f mem
    pure $ OMem mem
  OLoc loc -> OLoc <$> f loc
  other -> pure other

memLocs :: Traversal' Mem Loc
memLocs = traversalVL $ \f mem@Mem {base, index} -> do
  base <- traverse f base
  index <- traverse f index
  pure $ mem {base, index}

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
