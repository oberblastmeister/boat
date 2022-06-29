module Boat.LL.Name
  ( Name,
    NameSource,
    infiniteNames,
    runNameSource,
    freshName,
  )
where

import Boat.Utils.Source (Source)
import Boat.Utils.Source qualified as Source
import Data.Infinite qualified as Infinite

type Name = ByteString

type NameSource = Source Name

infiniteNames :: Infinite.Infinite ByteString
infiniteNames = fromString . show <$> Infinite.from (0 :: Int)

runNameSource :: Eff (NameSource ': es) a -> Eff es a
runNameSource = Source.evalSource infiniteNames

freshName :: NameSource :> es => Eff es Name
freshName = Source.fresh
