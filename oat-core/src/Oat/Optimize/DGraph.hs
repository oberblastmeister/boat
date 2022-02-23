module Oat.Optimize.DGraph where

import Oat.Optimize.Block (Block, Node, Shape)
import Oat.Optimize.Graph (Body', Graph', NonLocal (..))

type DGraph :: Type -> Node -> Shape -> Shape -> Type
type DGraph f n e x = Graph' (DBlock f) n e x

type DBlock :: Type -> Node -> Shape -> Shape -> Type
data DBlock f n e x = DBlock f (Block n e x)

type DBody :: Type -> Node -> Type
type DBody f n = Body' (DBlock f) n

instance NonLocal n => NonLocal (DBlock f n) where
  entryLabel (DBlock _ b) = entryLabel b
  successorLabels (DBlock _ b) = successorLabels b
