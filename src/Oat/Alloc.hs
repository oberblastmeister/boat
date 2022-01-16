{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Oat.Alloc where

import qualified Control.Lens as L
import Oat.LL.AST (Ty)
import qualified Oat.LL.AST as LL.AST
import Oat.LensOperators
import qualified Oat.X86.AST as X86

data Loc
  = LVoid
  | LReg !X86.Reg
  | LStack !Int
  | LLab !ByteString
  deriving (Show, Eq)

data Operand
  = Null
  | Const !Int64
  | Gid !ByteString
  | Loc !Loc
  deriving (Show, Eq)

data Ins
  = ILab !Loc
  | PMov [SMove] -- parallel move
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
  { _loc :: !Loc,
    _ty :: Ty
  }

data LoadIns = LoadIns
  { _loc :: !Loc,
    _ty :: Ty,
    _arg :: Operand
  }

data BinOpIns = BinOpIns
  { _loc :: !Loc,
    _op :: !LL.AST.BinOp,
    _ty :: Ty,
    _arg1 :: !Operand,
    _arg2 :: !Operand
  }

data StoreIns = StoreIns
  { _ty :: Ty,
    _arg1 :: !Operand,
    _arg2 :: !Operand
  }

data IcmpIns = IcmpIns
  { _loc :: !Loc,
    _cmpOp :: !LL.AST.CmpOp,
    _ty :: Ty,
    _arg1 :: !Operand,
    _arg2 :: !Operand
  }

data CallIns = CallIns
  { _loc :: !Loc,
    _ty :: Ty,
    _args :: [(Ty, Operand)]
  }

data BitcastIns = BitcastIns
  { _loc :: !Loc,
    _from :: Ty,
    _arg :: !Operand,
    _to :: Ty
  }

data GepIns = GepIns
  { _loc :: !Loc,
    _ty :: Ty,
    _arg :: !Operand,
    _args :: [Operand]
  }

data SMove = SMove
  { _loc :: !Loc,
    _ty :: Ty,
    _arg :: !Operand
  }

data RetIns = RetIns
  { _ty :: Ty,
    _arg :: Maybe Operand
  }

data CbrIns = CbrIns
  { _arg :: !Operand,
    _loc1 :: !Loc,
    _loc2 :: !Loc
  }

newtype FunBody = FunBody [Ins]

L.makeFieldsNoPrefix ''AllocaIns
L.makeFieldsNoPrefix ''LoadIns
L.makeFieldsNoPrefix ''BinOpIns
L.makeFieldsNoPrefix ''StoreIns
L.makeFieldsNoPrefix ''IcmpIns
L.makeFieldsNoPrefix ''CallIns
L.makeFieldsNoPrefix ''BitcastIns
L.makeFieldsNoPrefix ''GepIns
L.makeFieldsNoPrefix ''SMove
L.makeFieldsNoPrefix ''CbrIns
L.makeFieldsNoPrefix ''RetIns

test :: LoadIns -> Operand
test ins = ins ^. arg

operands :: L.Traversal' Ins Operand
operands = L.traversal go
  where
    go f = \case
      PMov movs -> do
        res <-
          traverse
            ( \smov@SMove {_arg} -> do
                arg <- f _arg
                pure (smov {_arg = arg} :: SMove)
            )
            movs
        pure $ PMov res
      BinOp ins@BinOpIns {_arg1, _arg2} -> do
        _arg1 <- f _arg1
        _arg2 <- f _arg2
        pure $ BinOp ins {_arg1, _arg2}
      Load ins@LoadIns {_arg} -> do
        _arg <- f _arg
        pure $ Load ins {_arg}
      Store ins@StoreIns {_arg1, _arg2} -> do
        _arg1 <- f _arg1
        _arg2 <- f _arg2
        pure $ Store ins {_arg1, _arg2}
      Icmp ins@IcmpIns {_arg1, _arg2} -> do
        _arg1 <- f _arg1
        _arg2 <- f _arg2
        pure $ Icmp ins {_arg1, _arg2}
      Call ins@CallIns {_args} -> do
        _args <- traverse (\(ty, operand) -> (ty,) <$> f operand) _args
        pure $ Call ins {_args}
      Bitcast ins@BitcastIns {_arg} -> do
        _arg <- f _arg
        pure $ Bitcast ins {_arg}
      Gep ins@GepIns {_arg, _args} -> do
        _arg <- f _arg
        _args <- traverse f _args
        pure $ Gep ins {_arg, _args}
      Cbr ins@CbrIns {_arg} -> do
        _arg <- f _arg
        pure $ Cbr ins {_arg}
      other -> pure other
