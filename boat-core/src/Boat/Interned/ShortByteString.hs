module Boat.Interned.ShortByteString where

import Boat.Interned.Internal
import Text.Show qualified

data IShortByteString = IShortByteString
  { iShortByteStringId :: {-# UNPACK #-} !Int,
    unIShortByteString :: {-# UNPACK #-} !ShortByteString
  }

instance Show IShortByteString where
  show = Text.Show.show . (unIShortByteString)

instance Eq IShortByteString where
  (==) = (==) `on` (iShortByteStringId)

instance Ord IShortByteString where
  compare = compare `on` (iShortByteStringId)

instance IsString IShortByteString where
  fromString = fromShortByteString . fromString

instance Hashable IShortByteString where
  hashWithSalt salt = hashWithSalt salt . (iShortByteStringId)

instance Interned IShortByteString where
  type Uninterned IShortByteString = ShortByteString

  toInterned = IShortByteString
  intern = fromShortByteString
  unintern = (unIShortByteString)

fromShortByteString :: ShortByteString -> IShortByteString
fromShortByteString = intern' cache

cache :: Cache ShortByteString
cache = unsafeMkCache (Proxy @IShortByteString)
