module Boat.Utils.Misc
  ( inBetween,
    pathTail,
    concatToEither,
    maybeToRight,
    maybeToLeft,
    rightToMaybe,
    leftToMaybe,
    mVecFromList,
    timSortNEBy,
    vecSortBy,
    timSortBy,
    timSort,
    isPowerOf2,
    alignForward,
    show',
    (!>>),
    (<<!),
    (!>=>),
    (<=<!),
    (#.),
    (.#),
    accToVec,
    (<<$>>),
  )
where

import Acc (Acc)
import Acc qualified
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Range (Range (RangeP))
import Data.Text.Lazy qualified as LText
import Data.Vector qualified as VB
import Data.Vector.Algorithms.Tim qualified as Vector.Algorithms.Tim
import Data.Vector.Fusion.Bundle.Monadic qualified as Bundle
import Data.Vector.Fusion.Bundle.Size (Size (Unknown))
import Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import Data.Vector.Generic qualified as V
import Data.Vector.Generic.Mutable qualified as VM
import Data.Vector.Mutable qualified as VBM
import Data.Word (Word64)
import System.FilePath qualified as FilePath
import Text.Pretty.Simple (pShowNoColor)
import Prelude hiding (Map, imap)

inBetween :: Range -> IntMap a -> IntMap a
inBetween (RangeP start end) imap = imap''
  where
    (imap'', _) = IntMap.split end imap'
    (_, imap') = IntMap.split start imap

pathTail :: FilePath -> FilePath
pathTail = FilePath.joinPath . tail . FilePath.splitPath

concatToEither :: [a] -> [b] -> [Either a b]
concatToEither as bs = fmap Left as ++ fmap Right bs

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just a) = Left a
maybeToLeft b Nothing = Right b

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just b) = Right b
maybeToRight a Nothing = Left a

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

-- this is save because the created vector will never be used
mVecFromList :: PrimMonad m => [a] -> m (VBM.MVector (PrimState m) a)
mVecFromList = VB.unsafeThaw . VB.fromList

type Comparison e = e -> e -> Ordering

type VecSorter =
  forall (m :: Type -> Type) (v :: Type -> Type -> Type) e.
  (PrimMonad m, VM.MVector v e) =>
  Comparison e ->
  v (PrimState m) e ->
  m ()

vecSortBy :: VecSorter -> (a -> a -> Ordering) -> [a] -> [a]
vecSortBy sorter cmp as = VB.toList $
  VB.create $ do
    mv <- mVecFromList as
    sorter cmp mv
    pure mv

vecSortNEBy :: VecSorter -> (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
vecSortNEBy sorter cmp = NonEmpty.fromList . vecSortBy sorter cmp . NonEmpty.toList

timSortBy :: (a -> a -> Ordering) -> [a] -> [a]
timSortBy = vecSortBy Vector.Algorithms.Tim.sortBy

timSort :: Ord a => [a] -> [a]
timSort = timSortBy compare

timSortNEBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
timSortNEBy = vecSortNEBy Vector.Algorithms.Tim.sortBy

isPowerOf2 :: Word64 -> Bool
isPowerOf2 i = (i .&. (i - 1)) == 0

alignForward :: HasCallStack => Int64 -> Word64 -> Int
alignForward addr align
  | align <= 1 = error "mult cannot be less than 1"
  | not $ isPowerOf2 align = error "align must be a power of 2"
  | otherwise = undefined

show' :: Show a => a -> String
show' = LText.unpack . pShowNoColor

{-# INLINE (!>>) #-}
(!>>) :: (a -> b) -> (b -> c) -> a -> c
(!>>) f g = \a -> g $! f a

infixr 1 !>>

{-# INLINE (<<!) #-}
(<<!) :: (b -> c) -> (a -> b) -> a -> c
(<<!) f g = \a -> f $! g a

infixr 1 <<!

{-# INLINE (!>=>) #-}
(!>=>) :: Monad m => (t -> m a) -> (a -> m b) -> t -> m b
f !>=> g = \x -> f x >>= (g $!)

infixr 1 !>=>

{-# INLINE (<=<!) #-}
(<=<!) :: Monad m => (a -> m b) -> (t -> m a) -> t -> m b
f <=<! g = \x -> g x >>= (f $!)

infixr 1 <=<!

{-# INLINE (#.) #-}
(#.) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce

infixr 9 #.

{-# INLINE (.#) #-}
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _ = coerce f

infixl 8 .#

{-# INLINE accToVec #-}
accToVec :: V.Vector v a => Acc a -> v a
accToVec = V.unstream . accToBundle

{-# INLINE accToBundle #-}
accToBundle :: Monad m => Acc a -> Bundle.Bundle m v a
accToBundle acc = Bundle.fromStream (accToStream acc) Unknown

{-# INLINE accToStream #-}
accToStream :: Monad m => Acc a -> Stream m a
accToStream acc = Stream step acc
  where
    step acc = case Acc.uncons acc of
      Just (x, acc') -> pure $ Yield x acc'
      Nothing -> pure Done

{-# INLINE (<<$>>) #-}
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> (f (g a)) -> (f (g b))
(<<$>>) = fmap . fmap
