module Oat.LL.Name
  ( Name,
    NameSource,
    newNameSource,
    HasNameSource,
  )
where

import Data.Infinite qualified as Infinite
import Data.Source (Source)
import Data.Source qualified as Source

type Name = ByteString

type NameSource = Source Name

type HasNameSource s = Source.HasSource s Name

newNameSource :: NameSource
newNameSource = Source.new $ (fromString . show) <$> Infinite.from (0 :: Int)