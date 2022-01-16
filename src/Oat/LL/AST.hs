{-# LANGUAGE TemplateHaskell #-}

module Oat.LL.AST where

import qualified Control.Lens as L
import Oat.LL.Name (Name)

data Ty
  = Void
  | I1
  | I8
  | I64
  | TyPtr Ty
  | TyFun FunTy
  | TyNamed !Name
  | TyArray !Int Ty
  | TyStruct [Ty]
  deriving (Show, Eq)

data FunTy = FunTy
  { _args :: [Ty],
    _ret :: Ty
  }
  deriving (Show, Eq)

instance L.Plated Ty where
  plate f = \case
    TyPtr ty -> TyPtr <$> f ty
    TyStruct tys -> TyStruct <$> traverse f tys
    TyFun FunTy {_args, _ret} -> TyFun <$> (FunTy <$> traverse f _args <*> f _ret)
    other -> pure other

data Operand
  = Null
  | Const !Int64
  | Gid !Name
  | Temp !Name
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data CmpOp
  = Eq
  | Neq
  | Slt
  | Sle
  | Sgt
  | Sge
  deriving (Show, Eq)

data Ins
  = BinOp BinOpIns
  | Alloca AllocaIns
  | Load LoadIns
  | Store StoreIns
  | Icmp IcmpIns
  | Call CallIns
  | Bitcast BitcastIns
  | Gep GepIns
  deriving (Show, Eq)

data BinOpIns = BinOpIns
  { _op :: BinOp,
    _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }
  deriving (Show, Eq)

newtype AllocaIns = AllocaIns
  { _ty :: Ty
  }
  deriving (Show, Eq)

data LoadIns = LoadIns
  { _ty :: Ty,
    _arg :: Operand
  }
  deriving (Show, Eq)

data StoreIns = StoreIns
  { _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }
  deriving (Show, Eq)

data IcmpIns = IcmpIns
  { _op :: !CmpOp,
    _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }
  deriving (Show, Eq)

data CallIns = CallIns
  { _ty :: Ty,
    _fn :: Operand,
    _args :: [Operand]
  }
  deriving (Show, Eq)

data BitcastIns = BitcastIns
  { _from :: Ty,
    _arg :: Operand,
    _to :: Ty
  }
  deriving (Show, Eq)

data GepIns = GepIns
  { _ty :: Ty,
    _arg1 :: Operand,
    _arg2 :: Operand
  }
  deriving (Show, Eq)

data Terminator
  = Ret RetTerm
  | Br !Name
  | Cbr CbrTerm
  deriving (Show, Eq)

data RetTerm = RetTerm
  { _ty :: Ty,
    _arg :: Maybe Operand
  }
  deriving (Show, Eq)

data CbrTerm = CbrTerm
  { _arg :: Operand,
    _lab1 :: !Name,
    _lab2 :: !Name
  }
  deriving (Show, Eq)

data Block = Block
  { _ins :: [Named Ins],
    _terminator :: Named Terminator
  }
  deriving (Show, Eq)

data LabBlock = LabBlock
  { _lab :: !Name,
    _block :: Block
  }
  deriving (Show, Eq)

data FunBody = FunBody
  { _entry :: Block,
    _labeled :: [LabBlock]
  }
  deriving (Show, Eq)

data FunDecl = FunDecl
  { _funTy :: FunTy,
    _params :: [Name],
    _cfg :: FunBody
  }
  deriving (Show, Eq)

data Named a
  = Named !Name a
  | Do a
  deriving (Show, Eq, Ord, Generic)

instance Hashable a => Hashable (Named a)

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
  deriving (Show, Eq)

data GDecl = GDecl Ty GInit
  deriving (Show, Eq)

data Prog = Prog
  { _tyDecls :: [Named Ty],
    _globalDecls :: [Named GDecl],
    _funDecls :: [Named FunDecl],
    _externalDecls :: [Named Ty]
  }
  deriving (Show, Eq)

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