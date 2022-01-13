{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Oat.LL.AST where

import qualified Control.Lens as L
import Control.Lens.Operators
import Oat.LL.Name (Name)
import qualified Oat.X86.AST as X86

data Loc r
  = LVoid
  | LReg !r
  | LStack !Int
  | LLab !ShortByteString

data AllocStatus = Abstract | Alloc

type family TempKind s r where
  TempKind 'Alloc r = Loc r
  TempKind 'Abstract _ = Name

data Ty
  = Void
  | I1
  | I8
  | I64
  | TyPtr Ty
  | TyStruct [Ty]
  | TyFun FunTy
  | TyNamed !Name

data FunTy = FunTy
  { _args :: [Ty],
    _ret :: Ty
  }

data Operand s r
  = Null
  | Const !Int64
  | Gid !Name
  | Temp (TempKind s r)

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

data CmpOp
  = Eq
  | Neq
  | Slt
  | Sle
  | Sgt
  | Sge

data Ins s r
  = BinOp (BinOpIns s r)
  | Alloca AllocaIns
  | Load (LoadIns s r)
  | Store (StoreIns s r)
  | Icmp (IcmpIns s r)
  | Call (CallIns s r)
  | Bitcast (BitcastIns s r)
  | Gep (GepIns s r)

data BinOpIns s r = BinOpIns
  { _op :: BinOp,
    _ty :: Ty,
    _arg1 :: Operand s r,
    _arg2 :: Operand s r
  }

newtype AllocaIns = AllocaIns
  { _ty :: Ty
  }

data LoadIns s r = LoadIns
  { _ty :: Ty,
    _arg :: Operand s r
  }

data StoreIns s r = StoreIns
  { _ty :: Ty,
    _arg1 :: Operand s r,
    _arg2 :: Operand s r
  }

data IcmpIns s r = IcmpIns
  { _op :: !CmpOp,
    _ty :: Ty,
    _arg1 :: Operand s r,
    _arg2 :: Operand s r
  }

data CallIns s r = CallIns
  { _ty :: Ty,
    _fn :: Operand s r,
    _args :: [(Ty, Operand s r)]
  }

data BitcastIns s r = BitcastIns
  { _from :: Ty,
    _arg :: Operand s r,
    _to :: Ty
  }

data GepIns s r = GepIns
  { _ty :: Ty,
    _arg :: Operand s r,
    _args :: [Operand s r]
  }

data Terminator s r
  = Ret (RetTerm s r)
  | Br !Name
  | Cbr (CbrTerm s r)

data RetTerm s r = RetTerm
  { _ty :: Ty,
    _arg :: Maybe (Operand s r)
  }

data CbrTerm s r = CbrTerm
  { _arg :: Operand s r,
    _lab1 :: !Name,
    _lab2 :: !Name
  }

data Block s r = Block
  { _ins :: [Named (Ins s r)],
    _terminator :: Named (Terminator s r)
  }

data LabBlock s r = LabBlock
  { _lab :: !Name,
    _block :: Block s r
  }

data FunBody s r = FunBody
  { _entry :: Block s r,
    _labeled :: [LabBlock s r]
  }

data FunDecl s r = FunDecl
  { _funTy :: FunTy,
    _params :: [Name],
    _cfg :: FunBody s r
  }

data Named a
  = Named !Name a
  | Do a

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

data GDecl = GDecl Ty GInit

data Prog s r = Prog
  { _tyDecls :: [Named Ty],
    _globalDecls :: [Named GDecl],
    _funDecls :: [Named (FunDecl s r)],
    _externalDecls :: [Named Ty]
  }

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
L.makePrisms ''Operand

type family Test a where
  Test 'True = Int
  Test 'False = Bool

data Thing a = Thing (Test a)

test :: L.Traversal (Thing a) (Thing a') (Test a) (Test a')
test = L.traversal go
  where
    go f (Thing t) = Thing <$> f t

thing :: Thing 'True
thing = Thing 1

thing' :: Thing 'False
thing' = thing & test .~ True

operands :: L.Traversal (Ins s r) (Ins s' r) (Operand s r) (Operand s' r)
operands = L.traversal go
  where
    go :: forall f s r s'. Applicative f => ((Operand s r) -> f (Operand s' r)) -> (Ins s r) -> f (Ins s' r)
    go f = \case
      BinOp ins@BinOpIns {_arg1, _arg2} -> do
        _arg1 <- f _arg1
        _arg2 <- f _arg2
        pure $ BinOp ins {_arg1, _arg2}
      Alloca ins -> pure $ Alloca ins
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
      Call ins@CallIns {_fn, _args} -> do
        _fn <- f _fn
        _args <- traverse (\(ty, operand) -> (ty,) <$> f operand) _args
        pure $ Call ins {_fn, _args}
      Bitcast ins@BitcastIns {_arg} -> do
        _arg <- f _arg
        pure $ Bitcast ins {_arg}
      Gep ins@GepIns {_arg, _args} -> do
        _arg <- f _arg
        _args <- traverse f _args
        pure $ Gep ins {_arg, _args}

-- doMatch :: Operand 'Alloc X86.Reg -> ()
-- doMatch = \case

doesInsAssign :: Ins s r -> Bool
doesInsAssign (Call CallIns {_ty = Void}) = False
doesInsAssign (Store _) = False
doesInsAssign _ = True

data Change a = Change {field :: [(Int, a)]}

change = Change [(1, True)]

change' = change {field = [(1 :: Int, 1 :: Int)]}