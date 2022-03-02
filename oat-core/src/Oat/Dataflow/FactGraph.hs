{-# LANGUAGE QuantifiedConstraints #-}

module Oat.Dataflow.FactGraph
  ( FactGraph,
    FactBlock (FactBlock),
    FactBody,
    spliceLeft,
    spliceRight,
    removeFacts,
  )
where

import Oat.Dataflow.Block (Block, Node)
import Oat.Dataflow.Block qualified as Block
import Oat.Dataflow.Graph (Body', Graph, Graph', NonLocal (..))
import Oat.Dataflow.Graph qualified as Graph
import Oat.Dataflow.Shape (Shape (..))

type FactGraph :: Type -> Node -> Shape -> Shape -> Type
type FactGraph f n e x = Graph' (FactBlock f) n e x

type FactBlock :: Type -> Node -> Shape -> Shape -> Type
data FactBlock f n e x = FactBlock {fact :: !f, block :: !(Block n e x)}

deriving instance (forall e x. Show (n e x), Show f) => Show (FactBlock f n e x)

type FactBody :: Type -> Node -> Type
type FactBody f n = Body' (FactBlock f) n

instance NonLocal n => NonLocal (FactBlock f n) where
  entryLabel (FactBlock _ b) = entryLabel b
  successorLabels (FactBlock _ b) = successorLabels b

-- this prefers facts from the left
spliceLeft :: NonLocal n => FactGraph f n e a -> FactGraph f n a x -> FactGraph f n e x
spliceLeft = Graph.splice' appendFactBlock
  where
    appendFactBlock (FactBlock f b) (FactBlock _ b') = FactBlock f (b `Block.append` b')

spliceRight :: NonLocal n => FactGraph f n e a -> FactGraph f n a x -> FactGraph f n e x
spliceRight = Graph.splice' appendFactBlock
  where
    appendFactBlock (FactBlock _ b) (FactBlock f b') = FactBlock f (b `Block.append` b')


removeFacts :: FactGraph f n e x -> Graph n e x
removeFacts = Graph.mapBlocks (.block)
