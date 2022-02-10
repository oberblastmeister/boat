{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fclear-plugins #-}

module Oat.LL.Cfg where

import Data.Graph.Inductive qualified as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Oat.LL (Name)
import Oat.LL.AST as LL
import Effectful.State.Static.Local

data Cfg = Cfg
  { graph :: Gr LL.Block (),
    nodeMap :: Graph.NodeMap Name
  }
  deriving (Show, Eq)

$(makeFieldLabelsNoPrefix ''Cfg)

-- fromAst :: LL.FunBody -> Cfg
-- fromAst body = undefined
--   where
--     (_, (nodeMap, graph)) = Graph.run $ do
--       Graph.insMapNodesM
