{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.LL.Ir where

import Oat.LL.AST qualified as LL
import Oat.Dataflow qualified as Optimize
import Oat.Dataflow.Block (Shape (..))

type Block = Optimize.Block Inst

type Graph = Optimize.Graph Inst

data FunBody = FunBody
  { graph :: Graph O C
  }
  deriving (Show, Eq)

data Inst e x where
  Label :: Optimize.Label -> Inst C O
  Inst :: LL.Inst -> Inst O O
  Term :: Term -> Inst O C

deriving instance (Show (Inst e x))

deriving instance (Eq (Inst e x))

data Term
  = Ret LL.RetTerm
  | Br Optimize.Label
  | Cbr CbrTerm
  deriving (Show, Eq)

data CbrTerm = CbrTerm
  { ty :: LL.Ty,
    arg :: LL.Operand,
    lab1 :: Optimize.Label,
    lab2 :: Optimize.Label
  }
  deriving (Show, Eq)

instance Optimize.NonLocal Inst where
  entryLabel (Label l) = l
  successorLabels (Term term) = case term of
    Br l -> [l]
    Cbr CbrTerm {lab1, lab2} -> [lab1, lab2]
    Ret _ -> []
