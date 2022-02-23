{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Pair (Pair (..), toTuple) where

import Control.DeepSeq (NFData)
import Data.Data (Data)

data Pair a b = P a b
  deriving (Show, Eq, Data, Typeable, Generic)

instance (NFData a, NFData b) => NFData (Pair a b)

instance (Hashable a, Hashable b) => Hashable (Pair a b)

instance Field1 (Pair a b) (Pair a' b) a a' where
  _1 = lensVL (\f (P a b) -> (`P` b) <$> f a)

instance Field2 (Pair a b) (Pair a b') b b' where
  _2 = lensVL (\f (P a b) -> P a <$> f b)

toTuple :: Pair a b -> (a, b)
toTuple (P a b) = (a, b)