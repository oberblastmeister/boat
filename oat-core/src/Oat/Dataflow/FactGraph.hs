{-# LANGUAGE QuantifiedConstraints #-}

module Oat.Dataflow.FactGraph
  ( FactGraph,
    FactBlock (FactBlock),
    FactBody,
    splice,
    (><),
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

deriving instance (forall e x. Eq (n e x), Eq f) => Eq (FactBlock f n e x)

type FactBody :: Type -> Node -> Type
type FactBody f n = Body' (FactBlock f) n

instance NonLocal n => NonLocal (FactBlock f n) where
  entryLabel (FactBlock _ b) = entryLabel b
  successorLabels (FactBlock _ b) = successorLabels b

-- this prefers facts from the left
splice :: NonLocal n => FactGraph f n e a -> FactGraph f n a x -> FactGraph f n e x
splice = Graph.splice' appenFactBlock
  where
    appenFactBlock (FactBlock f b) (FactBlock _ b') = FactBlock f (b `Block.append` b')

(><) :: NonLocal n => FactGraph f n e a -> FactGraph f n a x -> FactGraph f n e x
(><) = splice

infixl 5 ><

removeFacts :: FactGraph f n e x -> Graph n e x
removeFacts = Graph.mapBlocks (.block)
