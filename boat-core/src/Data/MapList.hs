{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.MapList
  ( MapList,
    toMap,
    insert,
    keysSet,
    keys,
    orderedSetKeys,
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import GHC.Exts (IsList (Item))
import Boat.TH (addUnderscoreLenses, getterFieldLabels)

data MapList k v = MapList
  { map :: !(HashMap k v),
    list :: [(k, v)]
  }
  deriving (Generic, Data, Typeable)

$(makeFieldLabelsWith getterFieldLabels ''MapList)
$(makeLensesWith addUnderscoreLenses ''MapList)

instance (Show k, Show v) => Show (MapList k v) where
  show MapList {list} = show list

instance (Eq k, Eq v) => Eq (MapList k v) where
  MapList {map} == MapList {map = map'} = map == map'

instance (Eq k, Hashable k) => Semigroup (MapList k v) where
  MapList {map, list} <> MapList {map = map', list = list'} =
    MapList
      { map = map <> map',
        list = list <> list'
      }

instance (NFData k, NFData v) => NFData (MapList k v)

instance AsEmpty (MapList k v) where
  _Empty = nearly (MapList HashMap.empty []) (null . (.list))

instance (Eq k, Hashable k) => IsList (MapList k v) where
  type Item (MapList k v) = (k, v)

  toList = view _list
  fromList = foldl' (\ml (k, v) -> insert k v ml) Empty

toMap :: MapList k v -> HashMap k v
toMap = view _map

insert :: (Eq k, Hashable k) => k -> v -> MapList k v -> MapList k v
insert key val ml@MapList {map, list} =
  if has (ix key) map
    then ml
    else
      MapList
        { map = map & at key ?~ val,
          list = (key, val) : list
        }

keys :: MapList k v -> [k]
keys = (^.. #list % traversed % _1)

keysSet :: MapList k v -> HashSet k
keysSet = (^. #map % to HashMap.keysSet)

orderedSetKeys :: (Eq k, Hashable k) => HashSet k -> MapList k v -> [k]
orderedSetKeys set mapList = filter (\k -> has (ix k) set) (keys mapList)
