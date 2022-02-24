module Data.Strict
  ( Strictly (..),
  )
where

import Unsafe.Coerce (unsafeCoerce)

class Strictly a where
  data Strict a
  strict :: a -> Strict a
  unstrict :: Strict a -> a
  matchStrict :: Strict a -> a
  constructStrict :: a -> Strict a

instance Strictly (t1, t2) where
  data Strict (t1, t2) = StrictPair !t1 !t2
  strict x = unsafeCoerce $ case x of
    (!_, !_) -> x
  matchStrict = \case
    StrictPair t1 t2 -> (t1, t2)
  unstrict = unsafeCoerce
  constructStrict (x, y) = StrictPair x y

instance Strictly (t1, t2, t3) where
  data Strict (t1, t2, t3) = StrictT3 !t1 !t2 !t3
  strict x = unsafeCoerce $ case x of
    (!_, !_, !_) -> x
  matchStrict = \case
    StrictT3 x1 x2 x3 -> (x1, x2, x3)
  unstrict = unsafeCoerce
  constructStrict (x1, x2, x3) = StrictT3 x1 x2 x3

instance Strictly (t1, t2, t3, t4) where
  data Strict (t1, t2, t3, t4) = StrictT4 !t1 !t2 !t3 !t4
  strict x = unsafeCoerce $ case x of
    (!_, !_, !_, !_) -> x
  matchStrict = \case
    StrictT4 x1 x2 x3 x4 -> (x1, x2, x3, x4)
  unstrict = unsafeCoerce
  constructStrict (x1, x2, x3, x4) = StrictT4 x1 x2 x3 x4

instance Strictly (Maybe t) where
  data Strict (Maybe t) = StrictNothing | StrictJust !t
  strict x = unsafeCoerce $ case x of
    Nothing -> x
    Just !_ -> x
  matchStrict = \case
    StrictJust j -> Just j
    StrictNothing -> Nothing
  unstrict = unsafeCoerce
  constructStrict = \case
    Just j -> StrictJust j
    Nothing -> StrictNothing

instance Strictly (Either t1 t2) where
  data Strict (Either t1 t2) = StrictLeft !t1 | StrictRight !t2
  strict x = unsafeCoerce $ case x of
    Left !_ -> x
    Right !_ -> x
  matchStrict = \case
    StrictLeft l -> Left l
    StrictRight r -> Right r
  unstrict = unsafeCoerce
  constructStrict = \case
    Left l -> StrictLeft l
    Right r -> StrictRight r

-- pattern S :: Strictly t => t -> Strict t 
