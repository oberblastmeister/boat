{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.AST where

import Oat.Common
import Oat.LL.LexerWrapper
import Oat.LL.Name (Name)
import Optics

data Ty
  = Void
  | I1
  | I8
  | I64
  | TyPtr Ty
  | TyFun FunTy
  | TyNamed !Name
  | TyArray !Int Ty
  | TyStruct (NonEmpty Ty)
  deriving (Show, Eq)

data FunTy = FunTy
  { args :: NonEmpty Ty,
    ret :: Ty
  }
  deriving (Show, Eq)

type TyMap = HashMap Name Ty

lookupTy :: Name -> TyMap -> Ty
lookupTy name mp = mp ^. at name % unwrap (internalError $ "Could not find name " <> show name <> " in map")

-- note, for this to be valid, you must not change the type for TyNamed
-- also, this will panic if the HashMap does not contain the name
plateTy :: TyMap -> Traversal' Ty Ty
plateTy mp = traversalVL $ \f -> \case
  TyPtr ty -> TyPtr <$> f ty
  TyFun FunTy {args, ret} -> TyFun <$> (FunTy <$> traverse f args <*> f ret)
  TyNamed name -> f (lookupTy name mp) $> TyNamed name
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
  { op :: BinOp,
    ty :: Ty,
    arg1 :: Operand,
    arg2 :: Operand
  }
  deriving (Show, Eq)

newtype AllocaIns = AllocaIns
  { ty :: Ty
  }
  deriving (Show, Eq)

data LoadIns = LoadIns
  { ty :: Ty,
    arg :: Operand
  }
  deriving (Show, Eq)

data StoreIns = StoreIns
  { ty :: Ty,
    arg1 :: Operand,
    arg2 :: Operand
  }
  deriving (Show, Eq)

data IcmpIns = IcmpIns
  { op :: !CmpOp,
    ty :: Ty,
    arg1 :: Operand,
    arg2 :: Operand
  }
  deriving (Show, Eq)

data CallIns = CallIns
  { ty :: Ty,
    fn :: Operand,
    args :: [Operand]
  }
  deriving (Show, Eq)

data BitcastIns = BitcastIns
  { from :: Ty,
    arg :: Operand,
    to :: Ty
  }
  deriving (Show, Eq)

data GepIns = GepIns
  { ty :: Ty,
    arg1 :: Operand,
    arg2 :: Operand
  }
  deriving (Show, Eq)

data Terminator
  = Ret RetTerm
  | Br !Name
  | Cbr CbrTerm
  deriving (Show, Eq)

data RetTerm = RetTerm
  { ty :: Ty,
    arg :: Maybe Operand
  }
  deriving (Show, Eq)

data CbrTerm = CbrTerm
  { arg :: Operand,
    lab1 :: !Name,
    lab2 :: !Name
  }
  deriving (Show, Eq)

data Block = Block
  { ins :: [Named Ins],
    terminator :: Named Terminator
  }
  deriving (Show, Eq)

data LabBlock = LabBlock
  { lab :: !Name,
    block :: Block
  }
  deriving (Show, Eq)

data FunBody = FunBody
  { entry :: Block,
    labeled :: [LabBlock]
  }
  deriving (Show, Eq)

data FunDecl = FunDecl
  { funTy :: FunTy,
    params :: [Name],
    cfg :: FunBody
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

data GDecl = GDecl {ty :: Ty, gInit :: GInit}
  deriving (Show, Eq)

data Prog = Prog
  { tyDecls :: [Named Ty],
    globalDecls :: [Named GDecl],
    funDecls :: [Named FunDecl],
    externalDecls :: [Named Ty]
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''LoadIns
makeFieldLabelsNoPrefix ''StoreIns
makeFieldLabelsNoPrefix ''IcmpIns
makeFieldLabelsNoPrefix ''CallIns
makeFieldLabelsNoPrefix ''BitcastIns
makeFieldLabelsNoPrefix ''GepIns
makeFieldLabelsNoPrefix ''FunTy
makeFieldLabelsNoPrefix ''Block
makeFieldLabelsNoPrefix ''FunBody
makeFieldLabelsNoPrefix ''FunDecl
makeFieldLabelsNoPrefix ''Prog

doesInsAssign :: Ins -> Bool
doesInsAssign (Call CallIns {ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True

-- thing :: Bool
-- thing = True