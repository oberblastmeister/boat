{-# LANGUAGE TemplateHaskell #-}

module Oat.LL.AST where

import qualified Control.Lens as L
import Oat.LL.Name (Name)
import qualified Oat.X86.AST as X86

data Loc r
  = LVoid
  | LReg !r
  | LStack !Int
  | LLab !ShortByteString

data AllocStatus = Abstract | Alloc

type family Alloc s r where
  Alloc 'Alloc r = Loc r
  Alloc 'Abstract _ = Name

data Ty
  = Void
  | I1
  | I8
  | I64
  | TyPtr Ty
  | TyStruct [Ty]
  | TyFun FunTy
  | TyNamed !Name

data FunTy = FunTy
  { _args :: [Ty],
    _ret :: Ty
  }

data Operand
  = Null
  | Const !Int64
  | Gid !Name
  | Temp !Name

data BinOp
  = Add
  | Sub
  | Mul
  | Shl
  | Lshr
  | Ashr
  | And
  | Or
  | Xor

data CmpOp
  = Eq
  | Neq
  | Slt
  | Sle
  | Sgt
  | Sge

data Ins
  = BinOp BinOpIns
  | Alloca AllocaIns
  | Load LoadIns
  | Store StoreIns
  | Icmp IcmpIns
  | Call CallIns
  | Bitcast BitcastIns
  | Gep GepIns

data BinOpIns = BinOpIns
  { _op :: BinOp,
    _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }

newtype AllocaIns = AllocaIns
  { _ty :: Ty
  }

data LoadIns = LoadIns
  { _ty :: Ty,
    _arg :: Operand
  }

data StoreIns = StoreIns
  { _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }

data IcmpIns = IcmpIns
  { _op :: !CmpOp,
    _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }

data CallIns = CallIns
  { _ty :: Ty,
    _fn :: Operand,
    _args :: [Operand]
  }

data BitcastIns = BitcastIns
  { _from :: Ty,
    _arg :: Operand,
    _to :: Ty
  }

data GepIns = GepIns
  { _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }

data Terminator
  = Ret RetTerm
  | Br !Name
  | Cbr CbrTerm

data RetTerm = RetTerm
  { _ty :: Ty,
    _arg :: Maybe Operand
  }

data CbrTerm = CbrTerm
  { _arg :: Operand,
    _lab1 :: !Name,
    _lab2 :: !Name
  }

data Block = Block
  { _ins :: [Named Ins],
    _terminator :: Named Terminator
  }

data LabBlock = LabBlock
  { _lab :: !Name,
    _block :: Block
  }

data FunBody = FunBody
  { _entry :: Block,
    _labeled :: [LabBlock]
  }

data FunDecl = FunDecl
  { _funTy :: FunTy,
    _params :: [Name],
    _cfg :: FunBody
  }

data Named a
  = Named !Name a
  | Do a

pattern (:=) :: Name -> a -> Named a
pattern name := a = Named name a

{-# COMPLETE (:=) #-}

data GInit
  = GNull
  | GGid !Name
  | GInt
  | GString !Name
  | GArray [GDecl]
  | GStruct [GDecl]

data GDecl = GDecl Ty GInit

data Prog = Prog
  { _tyDecls :: [Named Ty],
    _globalDecls :: [Named GDecl],
    _funDecls :: [Named FunDecl],
    _externalDecls :: [Named Ty]
  }

L.makeFieldsNoPrefix ''LoadIns
L.makeFieldsNoPrefix ''StoreIns
L.makeFieldsNoPrefix ''IcmpIns
L.makeFieldsNoPrefix ''CallIns
L.makeFieldsNoPrefix ''BitcastIns
L.makeFieldsNoPrefix ''GepIns
L.makeFieldsNoPrefix ''FunTy
L.makeFieldsNoPrefix ''Block
L.makeFieldsNoPrefix ''FunBody
L.makeFieldsNoPrefix ''FunDecl
L.makeFieldsNoPrefix ''Prog
L.makePrisms ''Named

doesInsAssign :: Ins -> Bool
doesInsAssign (Call CallIns {_ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True