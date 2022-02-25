module Oat.Dataflow.GraphExtra where

import Oat.Dataflow.Block qualified as Block
import Oat.Dataflow.Block (Shape (..))
import Oat.Dataflow.Graph qualified as Graph
import Oat.Dataflow.LabelSet (LabelSet)
import Oat.Dataflow.Graph (NonLocal(..))

type Graph n e x = Graph.Graph' Block.Block n e x

type Body n = Graph.Body' Block.Block n

data Block n e x = Block
  { preds :: !LabelSet,
    block :: !(Block.Block n e x),
    succs :: !LabelSet
  }

-- adFactBlock :: NonLocal n => Block.Block n C C -> Body n -> Body n
-- adFactBlock block body = undefined
--   where
--     succs = successorLabels block
