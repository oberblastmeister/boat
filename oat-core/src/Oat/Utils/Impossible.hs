module Oat.Utils.Impossible
  ( Impossible (..),
    throwImpossible,
    impossible,
  )
where

import Control.Exception (Exception, throw)
import Oat.Utils.CallStack (CallStack, prettyCallStack, withCallerCallStack)

data Impossible
  = Impossible CallStack
  deriving (Typeable)

-- Identify all values of Impossible. We use Impossible as a stand-in for the empty type, so all
-- values are morally equal.
instance Eq Impossible where
  _ == _ = True

instance Show Impossible where
  show (Impossible loc) =
    unlines
      [ "Something impossible happened",
        "Location of the error: ",
        prettyCallStack loc
      ]

instance Exception Impossible

-- | Abort by throwing an \"impossible\" error. You should not use
-- this function directly. Instead use impossible
throwImpossible :: Impossible -> a
throwImpossible = throw

impossible :: HasCallStack => a
impossible = withCallerCallStack $ throwImpossible . Impossible
