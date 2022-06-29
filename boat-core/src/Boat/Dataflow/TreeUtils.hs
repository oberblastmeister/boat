module Boat.Dataflow.TreeUtils
  ( preOrder,
    postOrderF,
    preOrderF,
    postOrderDL,
    postOrder,
  )
where

import Data.DList (DList)
import Data.DList qualified as DList
import Data.Tree (Tree (Node))

preOrderF :: [Tree a] -> [a]
preOrderF = foldMap preOrder
{-# INLINE preOrderF #-}

postOrderF :: [Tree a] -> [a]
postOrderF = DList.toList . getDual . foldMap (Dual . postOrderDL)
{-# INLINE postOrderF #-}

preOrder :: Tree a -> [a]
preOrder = order (:)
{-# INLINE preOrder #-}

postOrder :: Tree a -> [a]
postOrder = DList.toList . postOrderDL
{-# INLINE postOrder #-}

postOrderDL :: Tree a -> DList a
postOrderDL = getDual . order (\x xs -> Dual $ DList.snoc (getDual xs) x)
{-# INLINE postOrderDL #-}

order :: Monoid m => (a -> m -> m) -> Tree a -> m
order (.:) = go
  where
    go (Node r ts) = r .: foldMap go ts
{-# INLINE order #-}
