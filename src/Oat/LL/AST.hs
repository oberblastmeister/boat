{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.AST where

import Data.ASCII (ASCII, ToASCII (toASCII))
import Data.Data (Data)
import Oat.Common
import Oat.LL.Name (Name)
import Optics
import Optics.Operators.Unsafe ((^?!))
import Prelude hiding (Const)

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
  { args :: [Ty],
    ret :: Ty
  }
  deriving (Show, Eq)

type TyMap = HashMap (ASCII ByteString) Ty

lookupTy :: ASCII ByteString -> TyMap -> Ty
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

data InstShape = Flat | Tree

data Operand :: InstShape -> Type where
  Null :: Operand s
  Const :: !Int -> Operand s
  Gid :: !Name -> Operand s
  Temp :: !Name -> Operand 'Flat
  TempTree :: !Name -> (Maybe (Inst 'Tree)) -> Operand 'Tree

deriving instance (Show (Operand s))

deriving instance (Eq (Operand s))

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

data InstS = SInst | STerm

type AnyInst s' = forall s. Inst' s s'

type Inst = Inst' 'SInst

type Term = Inst' 'STerm

data Inst' :: InstS -> InstShape -> Type where
  BinOp :: BinOpInst s -> Inst' 'SInst s
  Alloca :: AllocaInst -> Inst' 'SInst s
  Load :: LoadInst s -> Inst' 'SInst s
  Store :: StoreInst s -> Inst' 'SInst s
  Icmp :: IcmpInst s -> Inst' 'SInst s
  Call :: CallInst s -> Inst' 'SInst s
  Bitcast :: BitcastInst s -> Inst' 'SInst s
  Gep :: GepInst s -> Inst' 'SInst s
  Ret :: RetTerm s -> Inst' 'STerm s
  Br :: !Name -> Inst' 'STerm s
  Cbr :: CbrTerm s -> Inst' 'STerm s

deriving instance (Show (Inst' s s'))

deriving instance (Eq (Inst' s s'))

data BinOpInst s = BinOpInst
  { name :: Name,
    op :: BinOp,
    ty :: Ty,
    arg1 :: Operand s,
    arg2 :: Operand s
  }
  deriving (Show, Eq)

data AllocaInst = AllocaInst
  { name :: Name,
    ty :: Ty
  }
  deriving (Show, Eq)

data LoadInst s = LoadInst
  { name :: Name,
    ty :: Ty,
    arg :: Operand s
  }
  deriving (Show, Eq)

data StoreInst s = StoreInst
  { ty :: Ty,
    arg1 :: Operand s,
    arg2 :: Operand s
  }
  deriving (Show, Eq)

data IcmpInst s = IcmpInst
  { name :: Name,
    op :: !CmpOp,
    ty :: Ty,
    arg1 :: Operand s,
    arg2 :: Operand s
  }
  deriving (Show, Eq)

data CallInst s = CallInst
  { name :: Maybe Name,
    ty :: Ty,
    fn :: Operand s,
    args :: [(Ty, Operand s)]
  }
  deriving (Show, Eq)

data BitcastInst s = BitcastInst
  { name :: Name,
    from :: Ty,
    arg :: Operand s,
    to :: Ty
  }
  deriving (Show, Eq)

data GepInst s = GepInst
  { name :: Name,
    ty :: Ty,
    arg :: Operand s,
    args :: [Operand s]
  }
  deriving (Show, Eq)

data RetTerm s = RetTerm
  { ty :: Ty,
    arg :: Maybe (Operand s)
  }
  deriving (Show, Eq)

data CbrTerm s = CbrTerm
  { arg :: Operand s,
    lab1 :: !Name,
    lab2 :: !Name
  }
  deriving (Show, Eq)

data Block s = Block
  { inst :: [Inst s],
    term :: Term s
  }
  deriving (Show, Eq)

data LabBlock s = LabBlock
  { lab :: !Name,
    block :: Block s
  }
  deriving (Show, Eq)

data FunBody s = FunBody
  { entry :: Block s,
    labeled :: [LabBlock s]
  }
  deriving (Show, Eq)

data FunDecl s = FunDecl
  { funTy :: FunTy,
    params :: [Name],
    cfg :: FunBody s
  }
  deriving (Show, Eq)

data Named a
  = Named Name a
  | Do a
  deriving (Show, Eq, Data, Typeable, Generic)

pattern (:=) :: Name -> a -> Named a
pattern (:=) name a = Named name a

{-# COMPLETE (:=) #-}

type WithName a = (Name, a)

instance Hashable a => Hashable (Named a)

data GlobalInit
  = GlobalNull
  | GlobalGid !(ASCII ByteString)
  | GlobalInt !Int64
  | GlobalString !ByteString
  | GlobalArray [GlobalDecl]
  | GlobalStruct [GlobalDecl]
  deriving (Show, Eq)

data GlobalDecl = GlobalDecl {ty :: Ty, globalInit :: GlobalInit}
  deriving (Show, Eq)

-- data FunDecl = FunDecl {name :: Name, FunDecl}
-- data DeclKind
--   = DeclTy
--   | DeclGlobal
--   | DeclFun
--   | DeclExtern

data Decl s
  = DeclTy {name :: Name, ty :: Ty}
  | DeclGlobal {name :: Name, globalDecl :: GlobalDecl}
  | DeclFun {name :: Name, funDecl :: FunDecl s}
  | DeclExtern {name :: Name, ty :: Ty}

data Prog s = Prog
  { decls :: [Decl s]
  }

makeFieldLabelsNoPrefix ''LoadInst
makeFieldLabelsNoPrefix ''BinOpInst
makeFieldLabelsNoPrefix ''StoreInst
makeFieldLabelsNoPrefix ''IcmpInst
makeFieldLabelsNoPrefix ''CallInst
makeFieldLabelsNoPrefix ''BitcastInst
makeFieldLabelsNoPrefix ''GepInst
makeFieldLabelsNoPrefix ''FunTy
makeFieldLabelsNoPrefix ''Block
makeFieldLabelsNoPrefix ''FunBody
makeFieldLabelsNoPrefix ''FunDecl
makeFieldLabelsNoPrefix ''Prog
makeFieldLabelsNoPrefix ''LabBlock
makeFieldLabelsNoPrefix ''RetTerm
makeFieldLabelsNoPrefix ''CbrTerm

instName :: AffineTraversal' (Inst s) Name
instName = atraversalVL go
  where
    go :: AffineTraversalVL' (Inst s) Name
    go point f = \case
      BinOp inst -> BinOp <$> atraverseOf #name point f inst
      Load inst -> Load <$> atraverseOf #name point f inst
      Icmp inst -> Icmp <$> atraverseOf #name point f inst
      Call inst -> Call <$> atraverseOf (#name % _Just) point f inst
      Bitcast inst -> Bitcast <$> atraverseOf #name point f inst
      Gep inst -> Gep <$> atraverseOf #name point f inst
      other -> point other

instOperands :: Traversal (Inst' s sa) (Inst' s sb) (Operand sa) (Operand sb)
instOperands = traversalVL go
  where
    go :: TraversalVL (Inst' s sa) (Inst' s sb) (Operand sa) (Operand sb)
    go f = \case
      BinOp inst@BinOpInst {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ BinOp inst {arg1, arg2}
      Alloca inst -> pure $ Alloca inst
      Load inst@LoadInst {arg} -> do
        arg <- f arg
        pure $ Load inst {arg}
      Store inst@StoreInst {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ Store inst {arg1, arg2}
      Icmp inst@IcmpInst {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ Icmp inst {arg1, arg2}
      Call inst@CallInst {fn, args} -> do
        fn <- f fn
        args <- traverse (\(ty, operand) -> (ty,) <$> f operand) args
        pure $ Call inst {fn, args}
      Bitcast inst@BitcastInst {arg} -> do
        arg <- f arg
        pure $ Bitcast inst {arg}
      Gep inst@GepInst {arg, args} -> do
        arg <- f arg
        args <- traverse f args
        pure $ Gep inst {arg, args}
      Ret inst -> Ret <$> traverseOf (#arg % _Just) f inst
      Br name -> pure $ Br name
      Cbr inst -> Cbr <$> traverseOf #arg f inst

doesInsAssign :: Inst s -> Bool
doesInsAssign (Call CallInst {ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True

-- thing :: Bool
-- thing = True