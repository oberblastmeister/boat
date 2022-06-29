module Boat.LL.Pass.Dom where

import Data.Tree (Tree)
import Boat.Dataflow (Label)

data DomTree = DomTree
  { tree ::
      Tree
        ( -- parent DomTree
          [DomTree],
          Label
        )
  }
