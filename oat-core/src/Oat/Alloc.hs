{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Alloc where

import Data.ASCII (ASCII)
import Data.Int (Int64)
import Oat.LL.AST (Ty)
import Oat.LL.AST qualified as LL.AST
import Oat.X86.AST qualified as X86

data Loc
  = LReg !X86.Reg
  | LStack !Int
  | LLab !(ASCII ByteString)
  deriving (Show, Eq)

data Operand
  = Null
  | Const !Int64
  | Gid !(ASCII ByteString)
  | Loc !Loc
  deriving (Show, Eq)

data Inst
  = ILab !Loc
  | PMove [SMove] -- parallel move
  | BinOp BinOpIns
  | Alloca AllocaIns
  | Load LoadIns
  | Store StoreIns
  | Icmp IcmpIns
  | Call CallIns
  | Bitcast BitcastIns
  | Gep GepIns
  | Ret RetIns
  | Br !Loc
  | Cbr CbrIns

data AllocaIns = AllocaIns
  { loc :: !Loc,
    ty :: Ty
  }

data LoadIns = LoadIns
  { loc :: !Loc,
    ty :: Ty,
    arg :: Operand
  }

data BinOpIns = BinOpIns
  { loc :: !Loc,
    op :: !LL.AST.BinOp,
    ty :: Ty,
    arg1 :: !Operand,
    arg2 :: !Operand
  }

data StoreIns = StoreIns
  { ty :: Ty,
    arg1 :: !Operand,
    arg2 :: !Operand
  }

data IcmpIns = IcmpIns
  { loc :: !Loc,
    op :: !LL.AST.CmpOp,
    ty :: Ty,
    arg1 :: !Operand,
    arg2 :: !Operand
  }

data CallIns = CallIns
  { loc :: Maybe Loc,
    ty :: Ty,
    fn :: Operand,
    args :: [(Ty, Operand)]
  }

data BitcastIns = BitcastIns
  { loc :: !Loc,
    from :: Ty,
    arg :: !Operand,
    to :: Ty
  }

data GepIns = GepIns
  { loc :: !Loc,
    ty :: Ty,
    arg :: !Operand,
    args :: [Operand]
  }

data SMove = SMove
  { loc :: !Loc,
    ty :: Ty,
    arg :: !Operand
  }

data RetIns = RetIns
  { ty :: Ty,
    arg :: Maybe Operand
  }

data CbrIns = CbrIns
  { arg :: !Operand,
    loc1 :: !Loc,
    loc2 :: !Loc
  }

type FunBody = [Inst]

makeFieldLabelsNoPrefix ''AllocaIns
makeFieldLabelsNoPrefix ''LoadIns
makeFieldLabelsNoPrefix ''BinOpIns
makeFieldLabelsNoPrefix ''StoreIns
makeFieldLabelsNoPrefix ''IcmpIns
makeFieldLabelsNoPrefix ''CallIns
makeFieldLabelsNoPrefix ''BitcastIns
makeFieldLabelsNoPrefix ''GepIns
makeFieldLabelsNoPrefix ''SMove
makeFieldLabelsNoPrefix ''CbrIns
makeFieldLabelsNoPrefix ''RetIns

operands :: Traversal' Inst Operand
operands = traversalVL go
  where
    go f = \case
      PMove movs -> do
        res <-
          traverse
            ( \smov@SMove {arg} -> do
                arg <- f arg
                pure (smov {arg} :: SMove)
            )
            movs
        pure $ PMove res
      BinOp inst@BinOpIns {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ BinOp inst {arg1, arg2}
      Load inst@LoadIns {arg} -> do
        arg <- f arg
        pure $ Load inst {arg}
      Store inst@StoreIns {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ Store inst {arg1, arg2}
      Icmp inst@IcmpIns {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ Icmp inst {arg1, arg2}
      Call inst@CallIns {args} -> do
        args <- traverse (\(ty, operand) -> (ty,) <$> f operand) args
        pure $ Call inst {args}
      Bitcast inst@BitcastIns {arg} -> do
        arg <- f arg
        pure $ Bitcast inst {arg}
      Gep inst@GepIns {arg, args} -> do
        arg <- f arg
        args <- traverse f args
        pure $ Gep inst {arg, args}
      Cbr inst@CbrIns {arg} -> do
        arg <- f arg
        pure $ Cbr inst {arg}
      other -> pure other