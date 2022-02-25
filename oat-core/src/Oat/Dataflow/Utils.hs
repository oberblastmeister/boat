module Oat.Dataflow.Utils
  ( joinMaps,
  )
where

import Data.Map.Strict qualified as Map
import Oat.Dataflow.Run (ChangeFlag (..), FactPair (..), Join)

-- | It's common to represent dataflow facts as a map from variables
-- to some fact about the locations. For these maps, the join
-- operation on the map can be expressed in terms of the join on each
-- element of the codomain:
-- Not present in map => Bot
joinMaps :: Ord k => Join v -> Join (Map k v)
joinMaps join l FactPair {old, new} = Map.foldrWithKey' add (NoChange, old) new
  where
    add k new (ch, !joinMap) =
      case joinMap ^. at k of
        Nothing -> (SomeChange, joinMap & at k ?~ new)
        Just old -> case join l FactPair {new, old} of
          (SomeChange, v') -> (SomeChange, joinMap & at k ?~ v')
          (NoChange, _) -> (ch, joinMap)
