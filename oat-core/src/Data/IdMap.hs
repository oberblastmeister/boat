{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.IdMap where

import Data.IntMap.Strict qualified as IntMap
import Oat.TH (addUnderscoreLenses, getterFieldLabels)

data Pair a b = P !a !b

pairToTuple :: Pair a b -> (a, b)
pairToTuple (P a b) = (a, b)

data IdMap k v = IdMap
  { map :: !(HashMap k v),
    idMap :: !(IntMap (Pair k v)),
    ix :: !Int
  }

$(makeFieldLabelsWith getterFieldLabels ''IdMap)
$(makeLensesWith addUnderscoreLenses ''IdMap)

instance (Show k, Show v) => Show (IdMap k v) where
  show IdMap {idMap} = show $ (\(i, P k v) -> (i, k, v)) <$> IntMap.toList idMap
