{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.LL.Live where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.Optics qualified as Set
import Oat.Dataflow (Shape (..))
import Oat.Dataflow qualified as Dataflow
import Oat.Dataflow.Graph qualified as Graph
import Oat.LL.Ast qualified as LL
import Oat.LL.Ir qualified as Ir
import Oat.LL.Name qualified as LL
import Oat.Utils.Optics (unwrap)

type Live = Set LL.Name

lattice :: Dataflow.Lattice Live
lattice =
  Dataflow.Lattice
    { name = "Live variables",
      bot = Set.empty,
      join
    }
  where
    join label Dataflow.FactPair {old, new} = (changeFlag, res)
      where
        !_ = dbg $ "join: " ++ show label
        changeFlag = Dataflow.changeIf $ Set.size res > Set.size old
        res = new `Set.union` old

transfer :: Dataflow.BackwardTransfer Ir.Inst Live
transfer = Dataflow.backwardTransfer1 $ \inst f -> case inst of
  Ir.Label _ -> f
  Ir.Inst inst -> (Set.setOf (LL.instOperands % #_Temp) inst) `Set.union` f
  Ir.Term term -> case term of
    Ir.Br label -> fact f label
    Ir.Cbr Ir.CbrTerm {arg, lab1, lab2} ->
      Set.setOf #_Temp arg
        `Set.union` fact f lab1
        `Set.union` fact f lab2
  Ir.Ret LL.RetTerm {arg} -> Set.setOf (_Just % #_Temp) arg `Set.union` f
  where
    fact :: Dataflow.FactBase Live -> Dataflow.Label -> Live
    fact f l = f ^. at l % unwrap Set.empty

rewrite :: forall m. Monad m => Dataflow.BackwardRewrite m Ir.Inst Live
rewrite = Dataflow.backwardRewrite1 $ \inst live -> case inst of
  (Ir.Inst inst) -> case inst ^? LL.instName of
    Just name
      | hasn't (ix name) live -> pure $ Just Graph.empty
      | otherwise -> pure Nothing
    Nothing -> pure Nothing
  _ -> pure Nothing

pass :: Monad m => Dataflow.BackwardPass m Ir.Inst Live
pass = Dataflow.BackwardPass {lattice, transfer, rewrite}

run :: forall m. (Monad m) => Ir.FunBody -> m (Dataflow.FactGraph Live Ir.Inst O O, Live)
run funBody = Dataflow.runBackward pass Dataflow.NothingC funBody.graph mempty
