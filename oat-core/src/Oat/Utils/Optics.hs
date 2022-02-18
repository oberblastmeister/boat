module Oat.Utils.Optics
  ( parOver,
    evalOf,
    parOf,
    _neHead,
    onOf,
    swap,
  )
where

import Control.Parallel.Strategies qualified as Parallel
import Data.List.NonEmpty qualified as NonEmpty

swap :: (Is k An_AffineFold, Is k' A_Review) => Optic' k is t a -> Optic' k' is' t b -> Setter t t a b
swap o o' = sets $ \f x -> case x ^? o of
  Nothing -> x
  Just y -> o' # f y
{-# INLINE swap #-}

parOver :: Is k A_Traversal => Parallel.Strategy b -> Optic k is s t a b -> (a -> b) -> s -> t
parOver strat o f = Parallel.runEval . traverseOf o (Parallel.rparWith strat . f)
{-# INLINE parOver #-}

evalOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
evalOf = traverseOf
{-# INLINE evalOf #-}

parOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
parOf o strat = traverseOf o $ Parallel.rparWith strat
{-# INLINE parOf #-}

_neHead :: Lens' (NonEmpty a) a
_neHead = lens NonEmpty.head (\(_ :| as) a -> a :| as)

onOf :: Is k A_Getter => (a -> a -> c) -> Optic' k is s a -> (s -> s -> c)
onOf f o = f `on` (^. o)

infixl 0 `onOf`