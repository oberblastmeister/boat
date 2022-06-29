module Boat.Interned.Internal
  ( Interned (..),
    unsafeMkCache,
    intern',
    Cache,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import GHC.IO.Unsafe qualified

data CacheState a = CacheState
  { id :: {-# UNPACK #-} !Int,
    cache :: !(HashMap a Int)
  }

newtype Cache a = Cache (IORef (CacheState a))

class (Eq (Uninterned a), Hashable (Uninterned a), Eq a, Hashable a) => Interned a where
  type Uninterned a

  toInterned :: Int -> Uninterned a -> a
  intern :: Uninterned a -> a
  unintern :: a -> Uninterned a

seedId :: Int
seedId = 0

unsafeMkCache :: Interned a => Proxy a -> Cache (Uninterned a)
unsafeMkCache _p = Cache $ GHC.IO.Unsafe.unsafePerformIO $ IORef.newIORef $ CacheState {id = seedId, cache = mempty}

intern' :: Interned a => Cache (Uninterned a) -> Uninterned a -> a
intern' (Cache cache) un = toInterned id un
  where
    id = GHC.IO.Unsafe.unsafeDupablePerformIO $ IORef.atomicModifyIORef cache go
    go (CacheState i m) = case HashMap.lookup un m of
      Nothing -> (CacheState (i + 1) (HashMap.insert un i m), i)
      Just i' -> (CacheState i m, i')
