{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.AST where

import Oat.LL.Name (Name)
import qualified Optics as O
import Optics.Operators.Unsafe ((^?!))

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

-- note, for this to be valid, you must not change the type for TyNamed
-- also, this will panic if the HashMap does not contain the name
plateTy :: HashMap Name Ty -> O.Traversal' Ty Ty
plateTy mp = O.traversalVL $ \f -> \case
  TyPtr ty -> TyPtr <$> f ty
  TyFun FunTy {_args, _ret} -> TyFun <$> (FunTy <$> traverse f _args <*> f _ret)
  TyNamed name -> let ty = mp ^?! O.ix name in f ty $> TyNamed name
  TyArray sz ty -> TyArray sz <$> f ty
  TyStruct tys -> TyStruct <$> traverse f tys
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

O.makeFieldLabelsNoPrefix ''LoadIns
O.makeFieldLabelsNoPrefix ''StoreIns
O.makeFieldLabelsNoPrefix ''IcmpIns
O.makeFieldLabelsNoPrefix ''CallIns
O.makeFieldLabelsNoPrefix ''BitcastIns
O.makeFieldLabelsNoPrefix ''GepIns
O.makeFieldLabelsNoPrefix ''FunTy
O.makeFieldLabelsNoPrefix ''Block
O.makeFieldLabelsNoPrefix ''FunBody
O.makeFieldLabelsNoPrefix ''FunDecl
O.makeFieldLabelsNoPrefix ''Prog

doesInsAssign :: Ins -> Bool
doesInsAssign (Call CallIns {_ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True