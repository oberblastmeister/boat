{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.LL.ToIr where

import Control.Applicative (liftA2)
import Control.Source qualified as Source
import Effectful.State.Static.Local (State, get)
import Effectful.State.Static.Local.Optics (assign)
import Oat.LL.AST qualified as LL
import Oat.LL.Ir qualified as Ir
import Oat.LL.Name qualified as LL
import Oat.Dataflow (Label, LabelSource, Shape (..))
import Oat.Dataflow.Block qualified as Block
import Oat.Dataflow.Graph qualified as Graph

type Effs =
  '[ State (HashMap LL.Name Label),
     LabelSource
   ]

funBodyToIr :: Effs :>> es => LL.FunBody -> Eff es Ir.FunBody
funBodyToIr body = do
  irEntryBlock <- blockToIr body.entry
  irLabBlocks <-
    foldl'
      (liftA2 Graph.splice)
      (pure Graph.emptyClosed)
      ( map
          ( \labBlock ->
              Graph.middle <$> labeledBlockToIr labBlock
          )
          body.labeled
      )
  let graph = Graph.entry irEntryBlock `Graph.splice` irLabBlocks
  pure $ Ir.FunBody {graph}
  where

labeledBlockToIr :: Effs :>> es => LL.LabBlock -> Eff es (Ir.Block C C)
labeledBlockToIr labBlock = do
  label <- getLabel labBlock.lab
  irBlock <- blockToIr labBlock.block
  pure $ Ir.Label label `Block.cons` irBlock

blockToIr :: Effs :>> es => LL.Block -> Eff es (Ir.Block O C)
blockToIr block = do
  irTerm <- termToIr block.term
  pure $ irBlock `Block.snoc` Ir.Term irTerm
  where
    irBlock =
      foldl'
        (\irBlock inst -> irBlock `Block.snoc` Ir.Inst inst)
        Block.empty
        block.insts

termToIr :: Effs :>> es => LL.Term -> Eff es Ir.Term
termToIr (LL.Ret inst) = pure $ Ir.Ret inst
termToIr (LL.Br name) = Ir.Br <$> getLabel name
termToIr (LL.Cbr LL.CbrTerm {ty, arg, lab1, lab2}) = do
  lab1 <- getLabel lab1
  lab2 <- getLabel lab2
  pure $ Ir.Cbr Ir.CbrTerm {ty, arg, lab1, lab2}

getLabel :: Effs :>> es => LL.Name -> Eff es Label
getLabel name = do
  map <- get @(HashMap LL.Name Label)
  case map ^. at name of
    Nothing -> do
      label <- Source.fresh
      assign @(HashMap LL.Name Label) (at name) (Just label)
      pure label
    Just label -> pure label
