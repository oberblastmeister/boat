module Data.NeAcc
  ( module Data.NeAcc,
  )
where

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf))
import Data.Foldable (foldr')
import Data.List (foldl1')
import GHC.Exts (IsList (Item))

data NeAcc a
  = Leaf a
  | Branch !(NeAcc a) !(NeAcc a)

instance Functor NeAcc where
  {-# INLINE fmap #-}
  fmap f = \case
    Leaf a -> Leaf $ f a
    Branch l r -> go l r
      where
        go l r = case l of
          Leaf a -> Branch (Leaf $ f a) (fmap f r)
          Branch l1 l2 -> go l1 (Branch l2 r)

instance Traversable NeAcc where
  traverse f = \case
    Leaf a -> Leaf <$> f a
    Branch l r -> go l r
      where
        go l r = case l of
          Leaf a -> Branch . Leaf <$> f a <*> traverse f r
          Branch l1 l2 -> go l1 (Branch l2 r)

instance Show a => Show (NeAcc a) where
  show = show . toList

instance NFData a => NFData (NeAcc a) where
  rnf = \case
    Leaf a -> rnf a
    Branch l r -> seq (rnf l) (rnf r)

instance NFData1 NeAcc where
  liftRnf rnfLeaf = rnfTree
    where
      rnfTree = \case
        Leaf a -> rnfLeaf a
        Branch l r -> seq (rnfTree l) (rnfTree r)

instance IsList (NeAcc a) where
  type Item (NeAcc a) = a

  {-# INLINE fromList #-}
  fromList = foldl1' (\acc leaf -> Branch leaf acc) . map Leaf

  {-# INLINE toList #-}
  toList = foldr (:) []

instance Applicative NeAcc where
  {-# INLINE pure #-}
  pure = Leaf

  {-# INLINE (<*>) #-}
  (<*>) = \case
    Branch a b -> \c -> Branch (a <*> c) (b <*> c)
    Leaf a -> fmap a

instance Foldable NeAcc where
  {-# INLINE foldr #-}
  foldr f z = \case
    Leaf a -> f a z
    Branch l r -> go l r
    where
      go l r = case l of
        Leaf a -> f a (foldr f z r)
        Branch l1 l2 -> go l1 (Branch l2 r)

  {-# INLINE foldr' #-}
  foldr' f z = \case
    Leaf a -> f a z
    Branch l r -> go l r
      where
        go l r = case r of
          Leaf a -> let !z' = f a z in foldr' f z' r
          Branch r1 r2 -> go r2 (Branch l r1)

  {-# INLINE foldl #-}
  foldl f z = \case
    Leaf a -> f z a
    Branch l r -> go l r
      where
        go l r = case r of
          Leaf a -> f (foldl f z r) a
          Branch r1 r2 -> go r2 (Branch l r1)

  {-# INLINE foldl' #-}
  foldl' f z = \case
    Leaf a -> f z a
    Branch l r -> go l r
      where
        go l r = case l of
          Leaf a -> let !z' = f z a in foldl' f z' r
          Branch l1 l2 -> go l1 (Branch l2 r)

instance Semigroup (NeAcc a) where
  {-# INLINE (<>) #-}
  (<>) = Branch

{-# INLINE cons #-}
cons :: a -> NeAcc a -> NeAcc a
cons a acc = Branch (Leaf a) acc

{-# INLINE snoc #-}
snoc :: NeAcc a -> a -> NeAcc a
snoc acc a = Branch acc (Leaf a)
