module OatPrelude
  ( module P,
    module X,
  )
where

import Control.Applicative as X (Alternative (..))
import Data.ByteString as X (ByteString)
import Data.ByteString.Short as X (ShortByteString)
import Data.Coerce as X
import Data.Foldable as X (foldl')
import Data.Function as X (on)
import Data.Functor as X (($>))
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.Hashable as X (Hashable (hash, hashWithSalt))
import Data.IntMap.Strict as X (IntMap)
import Data.IntSet as X (IntSet)
import Data.Kind as X (Constraint, Type)
import Data.Map.Strict as X (Map)
import Data.Monoid as X
import Data.Proxy as X (Proxy (..))
import Data.Sequence as X (Seq)
import Data.String as X (IsString (..))
import Data.Text as X (Text)
import Data.Typeable as X (Typeable)
import GHC.Generics as X (Generic, Generic1)
import GHC.Stack as X (HasCallStack)
import OatPrelude.Debug as X
import OatPrelude.Maybe as X
import OatPrelude.Monad as X
import Optics as X hiding (assign, assign', modifying, modifying', preuse, use)
import Prelude as P hiding (error, undefined)