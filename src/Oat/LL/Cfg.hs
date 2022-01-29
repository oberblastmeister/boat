{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.Cfg where

import qualified Data.Graph.Inductive as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import LLTree.AST (Block)
import Oat.LL (Name)
import Optics
import qualified Oat.LL.AST as LL

data Cfg = Cfg
  { graph :: Gr Block (),
    nodeMap :: Graph.NodeMap Name
  }
  deriving (Show, Eq)

$(makeFieldLabelsNoPrefix ''Cfg)

-- fromAst :: LL.FunBody -> Cfg
-- fromAst body = undefined 
--   where
--     (_, (nodeMap, graph)) = Graph.run $ do
--       Graph.insMapNodesM

