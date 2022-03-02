{-# LANGUAGE TemplateHaskell #-}

module Oat.Dataflow.LabelMap
  ( LabelMap,
    Label,
    unionWithKey,
    fromListWith,
    foldrWithKey',
    union,
    keys,
    singleton,
    mapWithKey,
  )
where

import Control.Arrow ((^>>))
import Control.DeepSeq (NFData)
import Data.IntMap.Strict qualified as IntMap
import GHC.Exts (IsList (Item))
import GHC.Exts qualified
import Oat.Dataflow.Label
import Oat.TH (addUnderscoreLenses)
import Prelude hiding (toList)

newtype LabelMap v = LabelMap {unLabelMap :: IntMap v}
  deriving (Show)
  deriving
    ( Eq,
      Ord,
      Semigroup,
      Monoid,
      NFData
    )
    via IntMap v
  deriving (Functor, Foldable) via IntMap
  deriving (Traversable)

$(makeLensesWith addUnderscoreLenses ''LabelMap)

type instance IxValue (LabelMap v) = v

type instance Index (LabelMap v) = Label

instance Ixed (LabelMap v)

instance At (LabelMap v) where
  at i = _unLabelMap % at (i.unLabel)

instance Eq v => AsEmpty (LabelMap v)

instance IsList (LabelMap v) where
  type Item (LabelMap v) = (Label, v)
  fromList = coerce @(IntMap v) @(LabelMap v) . IntMap.fromList . coerce
  toList = coerce @[(Int, v)] @[(Label, v)] . IntMap.toList . coerce

union :: LabelMap v -> LabelMap v -> LabelMap v
union m m' = LabelMap $ IntMap.union (coerce m) (coerce m')

unionWithKey :: (Label -> v -> v -> v) -> LabelMap v -> LabelMap v -> LabelMap v
unionWithKey f = coerce . IntMap.unionWithKey (coerce ^>> f) . coerce

fromListWith :: (v -> v -> v) -> [(Label, v)] -> LabelMap v
fromListWith f = coerce . IntMap.fromListWith f . coerce

foldrWithKey' :: forall a b. (Label -> a -> b -> b) -> b -> LabelMap a -> b
foldrWithKey' f b = IntMap.foldrWithKey' (coerce ^>> f) b . coerce

keys :: LabelMap v -> [Label]
keys = coerce . IntMap.keys . (.unLabelMap)

singleton :: Label -> v -> LabelMap v
singleton l = coerce . IntMap.singleton (coerce l)

mapWithKey :: (Label -> v -> v) -> LabelMap v -> LabelMap v
mapWithKey f = coerce . IntMap.mapWithKey (coerce ^>> f) . coerce
