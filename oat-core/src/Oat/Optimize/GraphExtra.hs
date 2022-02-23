module Oat.Optimize.GraphExtra where

import Oat.Optimize.Block qualified as Block
import Oat.Optimize.Block (Shape (..))
import Oat.Optimize.Graph qualified as Graph
import Oat.Optimize.LabelSet (LabelSet)
import Oat.Optimize.Graph (NonLocal(..))

type Graph n e x = Graph.Graph' Block.Block n e x

type Body n = Graph.Body' Block.Block n

data Block n e x = Block
  { preds :: !LabelSet,
    block :: !(Block.Block n e x),
    succs :: !LabelSet
  }

-- addBlock :: NonLocal n => Block.Block n C C -> Body n -> Body n
-- addBlock block body = undefined
--   where
--     succs = successorLabels block