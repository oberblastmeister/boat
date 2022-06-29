module Data.Acc
  ( module Data.Acc,
  )
where

import Data.NeAcc
import GHC.Exts (IsList (..))
import Prelude hiding (Empty, null)

data Acc a
  = Empty
  | Acc !(NeAcc a)
  deriving (Functor, Foldable, Traversable)

instance Show a => Show (Acc a) where
  show = show . toList

instance IsList (Acc a) where
  type Item (Acc a) = a

  {-# INLINE fromList #-}
  fromList = \case
    [] -> Empty
    xs -> Acc $ fromList xs

  {-# INLINE toList #-}
  toList = foldr (:) []

instance Applicative (Acc) where
  pure = Acc . Leaf

  {-# INLINE [1] (<*>) #-}
  (<*>) = \case
    Acc a -> \case
      Acc b ->
        Acc (a <*> b)
      Empty ->
        Empty
    Empty ->
      const Empty

instance Semigroup (Acc a) where
  {-# INLINE (<>) #-}
  Empty <> acc = acc
  acc <> Empty = acc
  Acc acc <> Acc acc' = Acc $ acc <> acc'

instance Monoid (Acc a) where
  {-# INLINE mempty #-}
  mempty = Empty

  {-# INLINE mappend #-}
  mappend = (<>)

{-# INLINE null #-}
null :: Acc a -> Bool
null Empty = True
null _ = False

instance AsEmpty (Acc a) where
  {-# INLINE _Empty #-}
  _Empty = nearly Empty null

{-# INLINE cons #-}
cons :: a -> Acc a -> Acc a
cons a acc = Acc (Leaf a) <> acc

{-# INLINE snoc #-}
snoc :: Acc a -> a -> Acc a
snoc acc a = acc <> Acc (Leaf a)
