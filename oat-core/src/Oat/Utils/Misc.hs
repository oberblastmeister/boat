module Oat.Utils.Misc
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
  timSort)
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Range (Range (RangeP))
import Data.Vector qualified as VB
import Data.Vector.Algorithms.Tim qualified as Vector.Algorithms.Tim
import Data.Vector.Generic.Mutable qualified as VM
import Data.Vector.Mutable qualified as VBM
import System.FilePath qualified as FilePath
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
