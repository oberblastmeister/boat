module Oat.Interned.Text
  ( IText (..),
    fromText,
  )
where

import qualified Data.Text as T
import Oat.Interned.Internal
import Prettyprinter (Pretty (pretty))
import qualified Text.Show

data IText = IText
  { iTextId :: {-# UNPACK #-} !Int,
    unIText :: {-# UNPACK #-} !Text
  }

instance Show IText where
  show = Text.Show.show . (.unIText)

instance Pretty IText where
  pretty = pretty . (.unIText)

instance Eq IText where
  (==) = (==) `on` (.iTextId)

instance Ord IText where
  compare = compare `on` (.iTextId)

instance IsString IText where
  fromString = fromText . T.pack

instance Hashable IText where
  hashWithSalt salt = hashWithSalt salt . (.iTextId)

instance Interned IText where
  type Uninterned IText = Text

  toInterned = IText
  intern = fromText
  unintern = (.unIText)

fromText :: Text -> IText
fromText = intern' cache

cache :: Cache Text
cache = unsafeMkCache (Proxy @IText)