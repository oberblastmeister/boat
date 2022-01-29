{-# LANGUAGE TemplateHaskell #-}

module Data.IdMap where

import Optics
import Prelude hiding (map)

data IdMap a = IdMap
  { _map :: !(IntMap a),
    _frees :: [Int],
    _key :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

$(makeLenses ''IdMap)

insert :: a -> IdMap a -> (Int, IdMap a)
insert a idMap@IdMap {_frees = free : _frees} = (free, (idMap & map % at free ?~ a) {_frees})