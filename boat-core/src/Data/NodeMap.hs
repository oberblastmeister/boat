{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.NodeMap
  ( run,
    run_,
    mkNodeMap,
    insert,
    insertM,
    insertMapGraph,
    insertMapGraphM,
    insertsMapGraphM,
  )
where

import Control.Monad.State.Lazy
import Data.Graph.Inductive (DynGraph)
import qualified Data.Graph.Inductive as Graph
import Boat.TH (getterFieldLabels, addUnderscoreLenses)
import qualified Data.HashMap.Strict as HashMap
import Optics
import Prelude hiding (map)

type NodeMapM a = State (NodeMap a)

type NodeMapGraphM a b c g = State (NodeMap a, g b c)

data NodeMap a = NodeMap
  { key :: !Int,
    map :: !(HashMap a Int)
  }
  deriving (Show, Eq, Ord, Generic)

$(makeFieldLabelsWith getterFieldLabels ''NodeMap)
$(makeLensesWith addUnderscoreLenses ''NodeMap)

type instance IxValue (NodeMap a) = Int

type instance Index (NodeMap a) = a

instance (Eq a, Hashable a) => Ixed (NodeMap a)

instance (Eq a, Hashable a) => At (NodeMap a) where
  at i = _map % at i

run :: NodeMap a -> g b c -> NodeMapGraphM a b c g r -> (r, NodeMap a, g b c)
run nmap graph m = (res, nmap', graph')
  where
    (res, (nmap', graph')) = runState m (nmap, graph)

run_ :: NodeMap a -> g b c -> NodeMapGraphM a b c g r -> (NodeMap a, g b c)
run_ nmap graph m = (nmap', graph')
  where
    (_, nmap', graph') = run nmap graph m

mkNodeMap :: NodeMap a
mkNodeMap = NodeMap {key = 0, map = HashMap.empty}

insert :: (Hashable a, Eq a) => a -> NodeMap a -> (Int, NodeMap a)
insert a nmap = (nmap ^. _key, nmap & _map % at a ?~ (nmap ^. _key) & _key %~ (+ 1))

insertM :: forall a. (Hashable a, Eq a) => a -> NodeMapM a Int
insertM = state . insert

insertMapGraph :: (Hashable a, Eq a, DynGraph g) => a -> b -> NodeMap a -> g b c -> (Int, NodeMap a, g b c)
insertMapGraph a b nmap graph = (node, nmap', graph')
  where
    graph' = Graph.insNode (node, b) graph
    (node, nmap') = insert a nmap

insertMapGraphM :: (Hashable a, Eq a, DynGraph g) => a -> b -> NodeMapGraphM a b c g Int
insertMapGraphM a b = do
  (nmap, graph) <- get
  let (node, nmap', graph') = insertMapGraph a b nmap graph
  put (nmap', graph')
  pure node

insertsMapGraphM :: (Hashable a, Eq a, DynGraph g) => [(a, b)] -> NodeMapGraphM a b c g [Int]
insertsMapGraphM = mapM $ uncurry insertMapGraphM
