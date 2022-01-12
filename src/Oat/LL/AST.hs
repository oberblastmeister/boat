{-# LANGUAGE TemplateHaskell #-}

module Oat.LL.AST where

import qualified Control.Lens as L
import Data.Interned.Internal.Text (InternedText)

data Ty
  = Void
  | I1
  | I8
  | I64
  | TyPtr Ty
  | TyStruct [Ty]
  | TyFun FunTy
  | TyNamed InternedText

data FunTy = FunTy
  { _args :: [Ty],
    _ret :: Ty
  }

data Operand
  = Null
  | Const Int64
  | Gid InternedText
  | Id InternedText

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
  | Alloca Ty
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
  { _op :: CmpOp,
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
  = Ret Ty (Maybe Operand)
  | Br InternedText
  | Cbr Operand InternedText InternedText

data Block = Block
  { _ins :: [Named Ins],
    _terminator :: Named Terminator
  }

data Cfg = Cfg
  { _entry :: Block,
    _labeled :: [(InternedText, Block)]
  }

data FunDecl = FunDecl
  { _funTy :: FunTy,
    _params :: [InternedText],
    _cfg :: Cfg
  }

data Named a
  = Named InternedText a
  | Do a

pattern (:=) :: InternedText -> a -> Named a
pattern name := a = Named name a

{-# COMPLETE (:=) #-}

data GInit
  = GNull
  | GGid InternedText
  | GInt
  | GString InternedText
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
L.makeFieldsNoPrefix ''Cfg
L.makeFieldsNoPrefix ''FunDecl
L.makeFieldsNoPrefix ''Prog

doesInsAssign :: Ins -> Bool
doesInsAssign (Call CallIns {_ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True