module Boat.Utils.Optics
  ( parOver,
    evalOf,
    parOf,
    _neHead,
    onOf,
    swap,
    fromOf,
    unwrap,
    leafChildrenOf,
    (%??),
  )
where

import Control.Parallel.Strategies qualified as Parallel
import Data.List.NonEmpty qualified as NonEmpty

{-# INLINE swap #-}
swap :: (Is k An_AffineFold, Is k' A_Review) => Optic' k is t a -> Optic' k' is' t b -> Setter t t a b
swap o o' = sets $ \f x -> case x ^? o of
  Nothing -> x
  Just y -> o' # f y

{-# INLINE parOver #-}
parOver :: Is k A_Traversal => Parallel.Strategy b -> Optic k is s t a b -> (a -> b) -> s -> t
parOver strat o f = Parallel.runEval . traverseOf o (Parallel.rparWith strat . f)

{-# INLINE evalOf #-}
evalOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
evalOf = traverseOf

{-# INLINE parOf #-}
parOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
parOf o strat = traverseOf o $ Parallel.rparWith strat

{-# INLINE _neHead #-}
_neHead :: Lens' (NonEmpty a) a
_neHead = lens NonEmpty.head (\(_ :| as) a -> a :| as)

{-# INLINE onOf #-}
onOf :: Is k A_Getter => (a -> a -> c) -> Optic' k is s a -> (s -> s -> c)
onOf f o = f `on` (^. o)

infixl 0 `onOf`

{-# INLINE fromOf #-}
fromOf :: Is k An_AffineFold => Optic' k is s a -> a -> Getter s a
fromOf o def = to $ \s -> case s ^? o of
  Just a -> a
  Nothing -> def

{-# INLINE unwrap #-}
unwrap :: a -> Lens' (Maybe a) a
unwrap def =
  lens
    (fromMaybe def)
    ( \m b -> case m of
        Just _ -> Just b
        Nothing -> Just def
    )

-- | get all the leaf children
{-# INLINE leafChildrenOf #-}
leafChildrenOf :: Is k A_Fold => Optic' k is a a -> a -> [a]
leafChildrenOf o = go
  where
    go a = foldMapOf o go a

{-# INLINE (%??) #-}
(%??) ::
  (Is k An_AffineFold, Is l A_Getter) =>
  Optic' k is s a ->
  Optic' l js (Maybe a) a' ->
  Getter s a'
(%??) o o' = to (\s -> s ^? o ^. o')
