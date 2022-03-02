{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.Ast
  ( module Oat.LL.Ast,
  )
where

import Data.Int (Int64)
import Data.MapList (MapList)
import Data.MapList qualified as MapList
import Data.Vector qualified as V
import Effectful.Reader.Static (Reader, ask)
import Effectful.Reader.Static qualified as Reader
import Effectful.Reader.Static.Optics (rview)
import Oat.LL.Name (Name)
import Oat.Utils.Optics (unwrap)
import Optics as O

data Module = Module
  { decls :: Vec Decl,
    declMap :: DeclMap
  }
  deriving (Show, Eq)

data DeclMap = DeclMap
  { tyDecls :: !(MapList Name Ty),
    globalDecls :: !(MapList Name GlobalDecl),
    funDecls :: !(MapList Name FunDecl),
    externDecls :: !(MapList Name Ty)
  }
  deriving (Show, Eq)

data Decl
  = DeclTy !Name Ty
  | DeclGlobal !Name GlobalDecl
  | DeclFun !Name FunDecl
  | DeclExtern !Name Ty
  deriving (Show, Eq)

data GlobalInit
  = GlobalNull
  | GlobalGid !Name
  | GlobalInt !Int64
  | GlobalString !ByteString
  | GlobalArray (Vec GlobalDecl)
  | GlobalStruct (Vec GlobalDecl)
  deriving (Show, Eq)

data GlobalDecl = GlobalDecl {ty :: Ty, globalInit :: GlobalInit}
  deriving (Show, Eq)

data Block = Block
  { insts :: Vec Inst,
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
    labeled :: Vec LabBlock
  }
  deriving (Show, Eq)

data FunDecl = FunDecl
  { funTy :: FunTy,
    params :: Vec Name,
    body :: FunBody
  }
  deriving (Show, Eq)

-- most of these types are just symbolic because we store everything in 8 bytes for simplicity
data Ty
  = Void
  | I1
  | I8
  | I64
  | TyPtr Ty
  | TyFun FunTy
  | TyNamed !Name
  | TyArray !Int Ty
  | TyStruct (Vec Ty)
  deriving (Show, Eq)

data FunTy = FunTy
  { args :: Vec Ty,
    ret :: Ty
  }
  deriving (Show, Eq)

type TyMap = HashMap Name Ty

data BinOp
  = Add
  | Sub
  | Mul
  | -- | shift left
    Shl
  | -- | logical shift right
    Lshr
  | -- | arithmetic shift right
    Ashr
  | And
  | Or
  | Xor
  deriving (Show, Eq)

data CmpOp
  = -- | equal
    Eq
  | -- | not equal
    Neq
  | -- | signed less than
    Slt
  | -- signed less than or equal
    Sle
  | -- | signed greater than
    Sgt
  | -- | signed greater than or equal
    Sge
  deriving (Show, Eq)

data Inst where
  BinOp :: BinOpInst -> Inst
  Alloca :: AllocaInst -> Inst
  Load :: LoadInst -> Inst
  Store :: StoreInst -> Inst
  Icmp :: IcmpInst -> Inst
  Call :: CallInst -> Inst
  Bitcast :: BitcastInst -> Inst
  Gep :: GepInst -> Inst
  Select :: SelectInst -> Inst
  Zext :: ZextInst -> Inst
  Sext :: SextInst -> Inst
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
    ty' :: Ty,
    arg :: Operand
  }
  deriving (Show, Eq)

data StoreInst = StoreInst
  { ty1 :: Ty,
    arg1 :: Operand,
    ty2 :: Ty,
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
    args :: Vec (Ty, Operand)
  }
  deriving (Show, Eq)

data BitcastInst = BitcastInst
  { name :: Name,
    ty1 :: Ty,
    arg :: Operand,
    ty2 :: Ty
  }
  deriving (Show, Eq)

data GepInst = GepInst
  { name :: Name,
    ty' :: Ty,
    ty :: Ty,
    arg :: Operand,
    args :: Vec (Ty, Operand)
  }
  deriving (Show, Eq)

data RetTerm = RetTerm
  { ty :: Ty,
    arg :: Maybe Operand
  }
  deriving (Show, Eq)

data CbrTerm = CbrTerm
  { ty :: Ty,
    arg :: Operand,
    lab1 :: !Name,
    lab2 :: !Name
  }
  deriving (Show, Eq)

data SelectInst = SelectInst
  { name :: !Name,
    condTy :: Ty,
    cond :: Operand,
    ty1 :: Ty,
    arg1 :: Operand,
    ty2 :: Ty,
    arg2 :: Operand
  }
  deriving (Show, Eq)

data ZextInst = ZextInst
  { name :: !Name,
    ty1 :: Ty,
    arg :: Operand,
    ty2 :: Ty
  }
  deriving (Show, Eq)

data SextInst = SextInst
  { name :: !Name,
    ty1 :: Ty,
    arg :: Operand,
    ty2 :: Ty
  }
  deriving (Show, Eq)

$(makeFieldLabelsNoPrefix ''Module)
$(makeFieldLabelsNoPrefix ''LoadInst)
$(makeFieldLabelsNoPrefix ''AllocaInst)
$(makeFieldLabelsNoPrefix ''BinOpInst)
$(makeFieldLabelsNoPrefix ''StoreInst)
$(makeFieldLabelsNoPrefix ''IcmpInst)
$(makeFieldLabelsNoPrefix ''CallInst)
$(makeFieldLabelsNoPrefix ''BitcastInst)
$(makeFieldLabelsNoPrefix ''GepInst)
$(makeFieldLabelsNoPrefix ''SelectInst)
$(makeFieldLabelsNoPrefix ''ZextInst)
$(makeFieldLabelsNoPrefix ''SextInst)
$(makeFieldLabelsNoPrefix ''FunTy)
$(makeFieldLabelsNoPrefix ''Block)
$(makeFieldLabelsNoPrefix ''FunBody)
$(makeFieldLabelsNoPrefix ''FunDecl)
$(makeFieldLabelsNoPrefix ''LabBlock)
$(makeFieldLabelsNoPrefix ''RetTerm)
$(makeFieldLabelsNoPrefix ''GlobalDecl)
$(makeFieldLabelsNoPrefix ''CbrTerm)
$(makeFieldLabelsNoPrefix ''DeclMap)
$(makePrismLabels ''Operand)
$(makePrismLabels ''BinOp)
$(makePrismLabels ''Inst)

declsToMap :: Vec Decl -> DeclMap
declsToMap =
  foldl'
    ( \declMap -> \case
        DeclTy name ty -> declMap & #tyDecls %~ MapList.insert name ty
        DeclGlobal name gDecl -> declMap & #globalDecls %~ MapList.insert name gDecl
        DeclFun name funDecl -> declMap & #funDecls %~ MapList.insert name funDecl
        DeclExtern name ty -> declMap & #externDecls %~ MapList.insert name ty
    )
    DeclMap {tyDecls = Empty, globalDecls = Empty, funDecls = Empty, externDecls = Empty}

instName :: AffineTraversal' Inst Name
instName = atraversalVL go
  where
    go :: AffineTraversalVL' Inst Name
    go point f = \case
      BinOp inst -> BinOp <$> atraverseOf #name point f inst
      Alloca inst -> Alloca <$> atraverseOf #name point f inst
      Load inst -> Load <$> atraverseOf #name point f inst
      inst@(Store _) -> point inst
      Icmp inst -> Icmp <$> atraverseOf #name point f inst
      Call inst -> Call <$> atraverseOf (#name % _Just) point f inst
      Bitcast inst -> Bitcast <$> atraverseOf #name point f inst
      Gep inst -> Gep <$> atraverseOf #name point f inst
      Select inst -> Select <$> atraverseOf #name point f inst
      Zext inst -> Zext <$> atraverseOf #name point f inst
      Sext inst -> Sext <$> atraverseOf #name point f inst

bodyBlocks :: Traversal' FunBody Block
bodyBlocks = traversalVL $ \f body -> do
  entry <- f $ body ^. #entry
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
        args <- traverseOf (each % _2) f args
        pure $ Gep inst {arg, args}
      Select inst@SelectInst {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ Select inst {arg1, arg2}
      Zext inst@ZextInst {arg} -> do
        arg <- f arg
        pure $ Zext inst {arg}
      Sext inst@SextInst {arg} -> do
        arg <- f arg
        pure $ Sext inst {arg}

termOperands :: Traversal' Term Operand
termOperands = traversalVL go
  where
    go :: TraversalVL' Term Operand
    go f = \case
      Ret inst -> Ret <$> traverseOf (#arg % _Just) f inst
      Br name -> pure $ Br name
      Cbr inst -> Cbr <$> traverseOf #arg f inst

operandName :: AffineTraversal' Operand Name
operandName = atraversalVL $ \point f -> \case
  Gid name -> Gid <$> f name
  Temp name -> Temp <$> f name
  other -> point other

doesInsAssign :: Inst -> Bool
doesInsAssign (Call CallInst {ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True

-- the size of the type in bytes
tySize :: Reader DeclMap :> es => Ty -> Eff es Int
tySize ty = do
  tyDecls <- rview @DeclMap (#tyDecls % #map)
  pure $ paraOf (plateTy tyDecls) go ty
  where
    go ty rs =
      case ty of
        Void -> 0
        -- this is not a valid type, we must only operate on i8* types
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

-- maximum call size of them in bytes
maxCallSize :: Reader DeclMap :> es => FunBody -> Eff es (Maybe Int)
maxCallSize body = do
  declMap <- ask @DeclMap
  pure $
    maximumOf
      ( bodyInsts
          % #_Call
          % #args
          % each
          % _1
          % O.to (runPureEff . Reader.runReader declMap . tySize)
      )
      body

lookupTy :: Name -> TyMap -> Ty
lookupTy name mp = mp ^. at name % unwrap (error $ "Could not find name " ++ show name ++ " in map")

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

funDeclTyParams :: FunDecl -> Vec (Name, Ty)
funDeclTyParams funDecl = V.zip (funDecl ^. #params) (funDecl ^. #funTy % #args)

declMapTyAt :: Name -> Fold DeclMap Ty
declMapTyAt name =
  #tyDecls % o
    `failing` #funDecls % o % #funTy % O.to TyFun
    `failing` #globalDecls % o % #ty
    `failing` #externDecls % o
  where
    o :: AffineFold (MapList Name v) v
    o = #map % ix name

isShiftOp :: BinOp -> Bool
isShiftOp Shl = True
isShiftOp Lshr = True
isShiftOp Ashr = True
isShiftOp _ = False

isNumTy :: Ty -> Bool
isNumTy I1 = True
isNumTy I8 = True
isNumTy I64 = True
isNumTy _ = False
