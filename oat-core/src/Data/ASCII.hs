{-# LANGUAGE FieldSelectors #-}

module Data.ASCII
  ( ToASCII (..),
    ASCII,
    unsafeToASCII,
    unsafeWithASCII,
    fromASCII,
  )
where

import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.Data (Data)
import Data.Word (Word8)
import Prettyprinter (Pretty (pretty))

class ToASCII s where
  toASCII :: s -> Maybe (ASCII s)

newtype ASCII s = ASCII {unASCII :: s}
  deriving (Eq, Ord, Data, Typeable, Generic, Hashable)

instance Show s => Show (ASCII s) where
  show = show . unASCII

instance ToASCII ByteString where
  toASCII bs =
    if isByteStringASCII bs
      then Just $ ASCII bs
      else Nothing

instance ToASCII String where
  toASCII s =
    if all Char.isAscii s
      then Just $ ASCII s
      else Nothing

instance Pretty s => Pretty (ASCII s) where
  pretty = pretty . unASCII

isByteStringASCII :: ByteString -> Bool
isByteStringASCII = ByteString.foldr (\b z -> isWordASCII b && z) True

isWordASCII :: Word8 -> Bool
isWordASCII = (<= 0x7f)

unsafeToASCII :: s -> ASCII s
unsafeToASCII = ASCII

fromASCII :: ASCII s -> s
fromASCII = unASCII

unsafeWithASCII :: (s -> s) -> ASCII s -> ASCII s
unsafeWithASCII f = unsafeToASCII . f . unASCII