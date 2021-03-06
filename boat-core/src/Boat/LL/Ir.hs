{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Boat.LL.Ir where

import Data.Some (Some1 (..))
import Language.Haskell.TH (Q, TExp, examineCode, Code)
import Boat.Dataflow (Shape (..))
import Boat.Dataflow qualified as Dataflow
import Boat.LL.Ast qualified as LL
import qualified Language.Haskell.TH as TH

type Block = Dataflow.Block Inst

type Graph = Dataflow.Graph Inst

data FunBody = FunBody
  { graph :: Graph O O
  }
  deriving (Show)

data Inst e x where
  Label :: Dataflow.Label -> Inst C O
  Inst :: LL.Inst -> Inst O O
  Term :: Term -> Inst O C
  -- Even though ret is a terminator in the ast,
  -- we make it open because there are no label targets for ret
  -- furthermore no other terminators can end a function
  Ret :: LL.RetTerm -> Inst O O

deriving instance (Show (Inst e x))

deriving instance (Eq (Inst e x))

data Term
  = Br Dataflow.Label
  | Cbr CbrTerm
  deriving (Show, Eq)

data CbrTerm = CbrTerm
  { ty :: LL.Ty,
    arg :: LL.Operand,
    lab1 :: Dataflow.Label,
    lab2 :: Dataflow.Label
  }
  deriving (Show, Eq)

instance Dataflow.NonLocal Inst where
  entryLabel (Label l) = l
  successorLabels (Term term) = case term of
    Br l -> [l]
    Cbr CbrTerm {lab1, lab2} -> [lab1, lab2]
