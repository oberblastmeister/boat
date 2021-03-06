{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Boat.LL.AstToIr where

import Effectful.State.Static.Local (State, get)
import Effectful.State.Static.Local.Optics (assign)
import Boat.Dataflow (Shape (..))
import Boat.Dataflow qualified as Dataflow
import Boat.Dataflow.Block qualified as Block
import Boat.Dataflow.Graph qualified as Graph
import Boat.LL.Ast qualified as LL
import Boat.LL.Ir qualified as Ir
import Boat.LL.Name qualified as LL
import Boat.Utils.Source qualified as Source
import Optics.Operators.Unsafe ((^?!))

type Effs =
  '[ State (HashMap LL.Name Dataflow.Label),
     State (Dataflow.LabelMap LL.Name),
     Dataflow.LabelSource
   ]

funBodyToIr :: Effs :>> es => LL.FunBody -> Eff es Ir.FunBody
funBodyToIr body = do
  graph <-
    blockToIr body.entry >>= \case
      Left onlyEntry -> pure $ Graph.block onlyEntry
      Right entry -> do
        let (labeled, lastLabeled) = case unsnoc body.labeled of
              Nothing -> error "must have two or more blocks if the entry block has a terminator"
              Just res -> res
        irLabBlocks <-
          foldM
            ( \(!graph) labBlock -> do
                converted <- labeledBlockToIr labBlock
                pure $ graph Graph.>< (Graph.block $! converted ^?! _Right)
            )
            (Graph.emptyClosed)
            labeled
        !lastIrLabBlock <- labeledBlockToIr lastLabeled <&> (^?! _Left) <&> Graph.block
        let graph = Graph.block entry Graph.>< irLabBlocks Graph.>< lastIrLabBlock
        pure graph
  pure Ir.FunBody {graph}

labeledBlockToIr :: Effs :>> es => LL.LabBlock -> Eff es (Either (Ir.Block C O) (Ir.Block C C))
labeledBlockToIr labBlock = do
  label <- getLabel labBlock.lab
  irBlock <- blockToIr labBlock.block
  pure $ bimap (Block.cons $ Ir.Label label) (Block.cons $ Ir.Label label) irBlock

blockToIr :: Effs :>> es => LL.Block -> Eff es (Either (Ir.Block O O) (Ir.Block O C))
blockToIr block = do
  termToIr block.term
    <&> bimap
      (Block.snoc irBlock . Ir.Ret)
      (Block.snoc irBlock . Ir.Term)
  where
    irBlock =
      foldl'
        (\irBlock inst -> irBlock `Block.snoc` Ir.Inst inst)
        Block.empty
        block.insts

termToIr :: Effs :>> es => LL.Term -> Eff es (Either LL.RetTerm Ir.Term)
termToIr (LL.Ret retTerm) = pure $ Left retTerm
termToIr (LL.Br name) = Right . Ir.Br <$> getLabel name
termToIr (LL.Cbr LL.CbrTerm {ty, arg, lab1, lab2}) = do
  lab1 <- getLabel lab1
  lab2 <- getLabel lab2
  pure $ Right $ Ir.Cbr Ir.CbrTerm {ty, arg, lab1, lab2}

getLabel :: Effs :>> es => LL.Name -> Eff es Dataflow.Label
getLabel name = do
  map <- get @(HashMap LL.Name Dataflow.Label)
  case map ^. at name of
    Nothing -> do
      label <- Source.fresh
      assign @(HashMap LL.Name Dataflow.Label) (at name) (Just label)
      assign @(Dataflow.LabelMap LL.Name) (at label) (Just name)
      pure label
    Just label -> pure label
