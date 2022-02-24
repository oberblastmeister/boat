module Oat.Dataflow.TreeUtils
  ( preOrder,
    postOrderF,
    preOrderF,
    postOrderDL,
    postOrder,
  )
where

import Data.DList (DList)
import Data.DList qualified as DL
import Data.Tree

preOrderF :: [Tree a] -> [a]
preOrderF = foldMap preOrder
{-# INLINE preOrderF #-}

postOrderF :: [Tree a] -> [a]
postOrderF = DL.toList . getDual . foldMap (Dual . postOrderDL)
{-# INLINE postOrderF #-}

preOrder :: Tree a -> [a]
preOrder = order (:)
{-# INLINE preOrder #-}

postOrder :: Tree a -> [a]
postOrder = DL.toList . postOrderDL
{-# INLINE postOrder #-}

postOrderDL :: Tree a -> DList a
postOrderDL = getDual . order (\x xs -> Dual $ DL.snoc (getDual xs) x)
{-# INLINE postOrderDL #-}

order :: Monoid m => (a -> m -> m) -> Tree a -> m
order (.:) = go
  where
    go (Node r ts) = r .: foldMap go ts
{-# INLINE order #-}
