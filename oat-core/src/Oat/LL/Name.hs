module Oat.LL.Name
  ( Name,
    NameSource,
    infiniteNames,
  )
where

import Data.Infinite qualified as Infinite
import Control.Source (Source)

type Name = ByteString

type NameSource = Source Name

infiniteNames :: Infinite.Infinite String
infiniteNames = fromString . show <$> Infinite.from (0 :: Int)