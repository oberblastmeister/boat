module Data.IxMap where

data Pair a b = P !a !b

data IxMap k v = IxMap
  { map :: !(HashMap k v),
    ixMap :: !(IntMap (Pair k v)),
    ix :: !Int
  }