module Data.ASCII
  ( ToASCII (..),
    ASCII,
    unsafeToASCII,
    fromASCII,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Data (Data)

class ToASCII s where
  toASCII :: s -> Maybe (ASCII s)

newtype ASCII s = ASCII {unASCII :: s}
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Hashable)

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

isByteStringASCII :: ByteString -> Bool
isByteStringASCII = ByteString.foldr (\b z -> isWordASCII b && z) True

isWordASCII :: Word8 -> Bool
isWordASCII = (<= 0x7f)

unsafeToASCII :: s -> ASCII s
unsafeToASCII = ASCII

fromASCII :: ASCII s -> s
fromASCII = unASCII