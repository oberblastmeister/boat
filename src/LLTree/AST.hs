module LLTree.AST where

import Oat.LL (Name)
import Oat.LL.AST (BinOp, CmpOp, FunTy, Ty)

-- data Inst
--   = BinOp

data InstS = SInst | STerm

type Inst = Inst' 'SInst

type Term = Inst' 'STerm

data Inst' :: InstS -> Type where
  BinOp :: BinOpInst -> Inst' 'SInst
  Alloca :: AllocaInst -> Inst' 'SInst
  Load :: LoadInst -> Inst' 'SInst
  Store :: StoreInst -> Inst' 'SInst
  Icmp :: IcmpInst -> Inst' 'SInst
  Call :: CallInst -> Inst' 'SInst
  Bitcast :: BitcastInst -> Inst' 'SInst
  Gep :: GepInst -> Inst' 'SInst
  Ret :: RetTerm -> Inst' 'STerm
  Br :: !Name -> Inst' 'STerm
  Cbr :: CbrTerm -> Inst' 'STerm

deriving instance (Show (Inst' s))

deriving instance (Eq (Inst' s))

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
  { inst :: [Inst],
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

-- %3 = %1 + %2

-- %4 = %3 + %9

-- %5 = %3 + 1
-- %1 = load m-1
-- %2 = %0 + %1
-- store %2 m-1

-- -- store (%0 + load %-1) (%-1)

-- %3 = load m-1
-- %4 = load %10
-- %5 = %3 + %4

-- -- %2 = %0 + %1
-- -- store %2 %1

-- -- %3 = load s
-- -- %4 = load s'
-- -- %5 = %3 + %4

-- %1 = load %-1
-- %2 = %0 + %1
-- store %2 %-1

-- -- store (%0 + load %-1) (%-1)

-- %3 = load %-1