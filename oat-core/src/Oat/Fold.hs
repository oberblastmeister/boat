-- until optics makes a new release
-- see https://github.com/well-typed/optics/pull/379
module Oat.Fold where

import Optics

paraOf :: Is k A_Fold => Optic' k is a a -> (a -> [r] -> r) -> a -> r
paraOf o f = go
  where
    go a = f a (go <$> toListOf o a)
{-# INLINE paraOf #-}