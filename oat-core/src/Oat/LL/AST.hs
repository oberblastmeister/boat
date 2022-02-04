{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.AST where

import Data.Data (Data)
import Data.Int (Int64)
import Data.Text qualified as T
import Oat.Common (internalError, unwrap)
import Oat.LL.Name (Name)
import Optics as O

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

type TyMap = HashMap Name Ty

lookupTy :: Name -> TyMap -> Ty
lookupTy name mp = mp ^. at name % unwrap (internalError $ "Could not find name " <> T.pack (show name) <> " in map")

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

data Inst where
  BinOp :: BinOpInst -> Inst
  Alloca :: AllocaInst -> Inst
  Load :: LoadInst -> Inst
  Store :: StoreInst -> Inst
  Icmp :: IcmpInst -> Inst
  Call :: CallInst -> Inst
  Bitcast :: BitcastInst -> Inst
  Gep :: GepInst -> Inst
  deriving (Show, Eq)

data Term where
  Ret :: RetTerm -> Term
  Br :: !Name -> Term
  Cbr :: CbrTerm -> Term
  deriving (Show, Eq)

data Operand where
  Const :: !Int -> Operand
  Gid :: !Name -> Operand
  Temp :: !Name -> Operand
  -- invariant, some instructions cannot be nested
  Nested :: Inst -> Operand
  deriving (Show, Eq)

data BinOpInst = BinOpInst
  { name :: Name,
    op :: BinOp,
    ty :: Ty,
    arg1 :: Operand,
    arg2 :: Operand
  }
  deriving (Show, Eq)

data AllocaInst = AllocaInst
  { name :: Name,
    ty :: Ty
  }
  deriving (Show, Eq)

data LoadInst = LoadInst
  { name :: Name,
    ty :: Ty,
    arg :: Operand
  }
  deriving (Show, Eq)

data StoreInst = StoreInst
  { ty :: Ty,
    arg1 :: Operand,
    arg2 :: Operand
  }
  deriving (Show, Eq)

data IcmpInst = IcmpInst
  { name :: Name,
    op :: !CmpOp,
    ty :: Ty,
    arg1 :: Operand,
    arg2 :: Operand
  }
  deriving (Show, Eq)

data CallInst = CallInst
  { name :: Maybe Name,
    ty :: Ty,
    fn :: Operand,
    args :: [(Ty, Operand)]
  }
  deriving (Show, Eq)

data BitcastInst = BitcastInst
  { name :: Name,
    from :: Ty,
    arg :: Operand,
    to :: Ty
  }
  deriving (Show, Eq)

data GepInst = GepInst
  { name :: Name,
    ty :: Ty,
    arg :: Operand,
    args :: [Operand]
  }
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
  { insts :: [Inst],
    term :: Term
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
  | GlobalGid !Name
  | GlobalInt !Int64
  | GlobalString !ByteString
  | GlobalArray [GlobalDecl]
  | GlobalStruct [GlobalDecl]
  deriving (Show, Eq)

data GlobalDecl = GlobalDecl {ty :: Ty, globalInit :: GlobalInit}
  deriving (Show, Eq)

data Decl
  = DeclTy {name :: Name, ty :: Ty}
  | DeclGlobal {name :: Name, globalDecl :: GlobalDecl}
  | DeclFun {name :: Name, funDecl :: FunDecl}
  | DeclExtern {name :: Name, ty :: Ty}

data Prog = Prog
  { decls :: [Decl]
  }

$(makeFieldLabelsNoPrefix ''LoadInst)
$(makeFieldLabelsNoPrefix ''AllocaInst)
$(makeFieldLabelsNoPrefix ''BinOpInst)
$(makeFieldLabelsNoPrefix ''StoreInst)
$(makeFieldLabelsNoPrefix ''IcmpInst)
$(makeFieldLabelsNoPrefix ''CallInst)
$(makeFieldLabelsNoPrefix ''BitcastInst)
$(makeFieldLabelsNoPrefix ''GepInst)
$(makeFieldLabelsNoPrefix ''FunTy)
$(makeFieldLabelsNoPrefix ''Block)
$(makeFieldLabelsNoPrefix ''FunBody)
$(makeFieldLabelsNoPrefix ''FunDecl)
$(makeFieldLabelsNoPrefix ''Prog)
$(makeFieldLabelsNoPrefix ''LabBlock)
$(makeFieldLabelsNoPrefix ''RetTerm)
$(makeFieldLabelsNoPrefix ''CbrTerm)
$(makePrismLabels ''Operand)
$(makePrismLabels ''Inst)

instName :: AffineTraversal' Inst Name
instName = atraversalVL go
  where
    go :: AffineTraversalVL' Inst Name
    go point f = \case
      BinOp inst -> BinOp <$> atraverseOf #name point f inst
      Load inst -> Load <$> atraverseOf #name point f inst
      Icmp inst -> Icmp <$> atraverseOf #name point f inst
      Call inst -> Call <$> atraverseOf (#name % _Just) point f inst
      Bitcast inst -> Bitcast <$> atraverseOf #name point f inst
      Gep inst -> Gep <$> atraverseOf #name point f inst
      other -> point other

bodyBlocks :: Traversal' FunBody Block
bodyBlocks = traversalVL $ \f body -> do
  entry <- f (body ^. #entry)
  labeled <- traverseOf (each % #block) f (body ^. #labeled)
  pure $ FunBody {entry, labeled}

bodyInsts :: Traversal' FunBody Inst
bodyInsts = bodyBlocks % #insts % traversed

instOperands :: Traversal' Inst Operand
instOperands = traversalVL go
  where
    go :: TraversalVL' Inst Operand
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

termOperands :: Traversal' Term Operand
termOperands = traversalVL go
  where
    go :: TraversalVL' Term Operand
    go f = \case
      Ret inst -> Ret <$> traverseOf (#arg % _Just) f inst
      Br name -> pure $ Br name
      Cbr inst -> Cbr <$> traverseOf #arg f inst

operandName :: AffineTraversal' Operand Name
operandName = atraversalVL go
  where
    go :: AffineTraversalVL' Operand Name
    go point f = \case
      Gid name -> Gid <$> f name
      Temp name -> Temp <$> f name
      other -> point other

doesInsAssign :: Inst -> Bool
doesInsAssign (Call CallInst {ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True

tySize :: TyMap -> Ty -> Int
tySize tyDecls =
  paraOf (plateTy tyDecls) go
  where
    go ty rs =
      case ty of
        Void -> 0
        I8 -> 0
        I1 -> 8
        I64 -> 8
        TyFun _ -> 0
        TyNamed _ -> size
        TyPtr _ -> 8
        TyArray n _ -> n * size
        TyStruct _ -> size
      where
        size = sum rs

maxCallSize :: TyMap -> FunBody -> Maybe Int
maxCallSize tyMap =
  maximumOf $
    bodyInsts
      % #_Call
      % #args
      % each
      % _1
      % O.to (tySize tyMap)