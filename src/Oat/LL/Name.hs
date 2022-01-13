module Oat.LL.Name
  ( mkName,
    Name (..),
  )
where

import Data.Char (isAscii)
import Data.Data (Data)

data Name
  = -- | a string name
    Name !ShortByteString
  | UnName !Int
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Hashable Name

-- | Using 'fromString` on non-ASCII strings will throw an error.
instance IsString Name where
  fromString s
    | all isAscii s = Name (fromString s)
    | otherwise =
      error
        ( "Oat.LL.Name.fromString: Only ASCII strings are automatically converted to LLVM names. "
            <> "Other strings need to be encoded to a `ShortByteString` using an arbitrary encoding."
        )

-- | Create a 'Name' based on an ASCII 'String'.
-- Non-ASCII strings will throw an error.
mkName :: String -> Name
mkName = fromString