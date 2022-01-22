{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Alloc where

import Oat.LL.AST (Ty)
import qualified Oat.LL.AST as LL.AST
import Oat.Wrappers.Lexer
import qualified Oat.X86.AST as X86
import Optics
import Optics.Operators
import Data.ASCII (ASCII)

data Loc
  = LVoid
  | LReg !X86.Reg
  | LStack !Int
  | LLab !(ASCII ByteString)
  deriving (Show, Eq)

data Operand
  = Null
  | Const !Int64
  | Gid !(ASCII ByteString)
  | Loc !Loc
  deriving (Show, Eq)

data Ins
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
    cmpOp :: !LL.AST.CmpOp,
    ty :: Ty,
    arg1 :: !Operand,
    arg2 :: !Operand
  }

data CallIns = CallIns
  { loc :: !Loc,
    ty :: Ty,
    arg :: Operand,
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

newtype FunBody = FunBody [Ins]

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

operands :: Traversal' Ins Operand
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
      BinOp ins@BinOpIns {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ BinOp ins {arg1, arg2}
      Load ins@LoadIns {arg} -> do
        arg <- f arg
        pure $ Load ins {arg}
      Store ins@StoreIns {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ Store ins {arg1, arg2}
      Icmp ins@IcmpIns {arg1, arg2} -> do
        arg1 <- f arg1
        arg2 <- f arg2
        pure $ Icmp ins {arg1, arg2}
      Call ins@CallIns {args} -> do
        args <- traverse (\(ty, operand) -> (ty,) <$> f operand) args
        pure $ Call ins {args}
      Bitcast ins@BitcastIns {arg} -> do
        arg <- f arg
        pure $ Bitcast ins {arg}
      Gep ins@GepIns {arg, args} -> do
        arg <- f arg
        args <- traverse f args
        pure $ Gep ins {arg, args}
      Cbr ins@CbrIns {arg} -> do
        arg <- f arg
        pure $ Cbr ins {arg}
      other -> pure other