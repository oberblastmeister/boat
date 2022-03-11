module Oat.LL.Pass.Dom where

import Data.Tree (Tree)
import Oat.Dataflow (Label)

data DomTree = DomTree
  { tree ::
      Tree
        ( -- parent DomTree
          [DomTree],
          Label
        )
  }
