module Oat.LL.Name
  ( Name,
    NameSource,
    infiniteNames,
    runNameSource,
  )
where

import Data.Infinite qualified as Infinite
import Control.Source (Source)
import qualified Control.Source as Source

type Name = ByteString

type NameSource = Source Name

infiniteNames :: Infinite.Infinite ByteString
infiniteNames = fromString . show <$> Infinite.from (0 :: Int)

runNameSource :: Eff (NameSource ': es) a -> Eff es a
runNameSource = Source.evalSource infiniteNames