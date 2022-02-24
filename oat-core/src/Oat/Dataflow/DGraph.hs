{-# LANGUAGE QuantifiedConstraints #-}

module Oat.Dataflow.DGraph
  ( DGraph,
    DBlock (DBlock),
    DBody,
    spliceAppend,
  )
where

import Oat.Dataflow.Block (Block, Node, Shape)
import Oat.Dataflow.Block qualified as Block
import Oat.Dataflow.Graph (Body', Graph', NonLocal (..))
import Oat.Dataflow.Graph qualified as Graph

type DGraph :: Type -> Node -> Shape -> Shape -> Type
type DGraph f n e x = Graph' (DBlock f) n e x

type DBlock :: Type -> Node -> Shape -> Shape -> Type
data DBlock f n e x = DBlock f (Block n e x)

deriving instance (forall e x. Show (n e x), Show f) => Show (DBlock f n e x)

deriving instance (forall e x. Eq (n e x), Eq f) => Eq (DBlock f n e x)

type DBody :: Type -> Node -> Type
type DBody f n = Body' (DBlock f) n

instance NonLocal n => NonLocal (DBlock f n) where
  entryLabel (DBlock _ b) = entryLabel b
  successorLabels (DBlock _ b) = successorLabels b

spliceAppend :: NonLocal n => DGraph f n e a -> DGraph f n a x -> DGraph f n e x
spliceAppend = Graph.splice' appendBlock
  where
    appendBlock (DBlock f b) (DBlock _ b') = DBlock f (b `Block.append` b')
