{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.IdSet
  ( IdSet,
    empty,
    toList,
    insert,
    delete,
    insertM,
    deleteM,
    insert_,
    insertM_,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.Infinite (Infinite ((::>)))
import Data.Infinite qualified as Infinite
import Data.IntMap.Strict qualified as IntMap
import Effectful.State.Static.Local
import Boat.TH (addUnderscoreLenses, getterFieldLabels)
import Prelude hiding (empty, null, toList)

data IdSet k = IdSet
  { set :: !(HashMap k Int),
    idSet :: !(IntMap k),
    source :: Infinite Int
  }
  deriving (Generic, Data, Typeable)

$(makeFieldLabelsWith getterFieldLabels ''IdSet)
$(makeLensesWith addUnderscoreLenses ''IdSet)

instance (Show k) => Show (IdSet k) where
  show = show . IntMap.toList . (.idSet)

emptyWith :: Infinite Int -> IdSet k
emptyWith source =
  IdSet
    { set = HashMap.empty,
      idSet = mempty,
      source
    }

empty :: IdSet k
empty = emptyWith $ Infinite.from 0

null :: IdSet k -> Bool
null set = HashMap.null set.set

instance (Eq k) => Eq (IdSet k) where
  (==) = (==) `on` (.idSet)

instance (NFData k) => NFData (IdSet k) where
  rnf IdSet {set, idSet} = rnf set `seq` rnf idSet

instance AsEmpty (IdSet k) where
  _Empty = nearly empty null

toList :: IdSet k -> [k]
toList = fmap snd . IntMap.toList . (.idSet)

takeSource :: IdSet k -> (Int, IdSet k)
takeSource set@IdSet {source = a ::> as} = (a, set {source = as})

insert :: (Eq k, Hashable k) => k -> IdSet k -> (IdSet k, Int)
insert k set = case set ^. #set % at k of
  Just i -> (set, i)
  Nothing -> do
    let (i, set') = takeSource set
    (set' & _set % at k ?~ i & _idSet % at i ?~ k, i)

insert_ :: (Eq k, Hashable k) => k -> IdSet k -> (IdSet k)
insert_ k = fst . insert k

insertM :: (State (IdSet k) :> es, Eq k, Hashable k) => k -> Eff es Int
insertM k = do
  set <- get
  let (set', i) = insert k set
  put set'
  pure i

insertM_ :: (State (IdSet k) :> es, Eq k, Hashable k) => k -> Eff es ()
insertM_ k = (const ()) <$> insertM_ k

delete :: (Eq k, Hashable k) => k -> IdSet k -> (IdSet k)
delete k set = case set ^. #set % at k of
  Just i -> set & _idSet % at i .~ Nothing & _source %~ (Infinite.cons i)
  Nothing -> set

deleteM :: (State (IdSet k) :> es, Eq k, Hashable k) => k -> Eff es ()
deleteM k = modify $ delete k
