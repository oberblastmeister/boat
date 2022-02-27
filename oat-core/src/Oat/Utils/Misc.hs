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
    timSort,
    isPowerOf2,
    alignForward,
    show',
    (!>>),
    (<<!),
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Range (Range (RangeP))
import Data.Text.Lazy qualified as LText
import Data.Vector qualified as VB
import Data.Vector.Algorithms.Tim qualified as Vector.Algorithms.Tim
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

alignForward :: Int64 -> Word64 -> Int
alignForward addr align
  | align <= 1 = error "mult cannot be less than 1"
  | not $ isPowerOf2 align = error "align must be a power of 2"
  | otherwise = undefined

show' :: Show a => a -> String
show' = LText.unpack . pShowNoColor

(!>>) :: (a -> b) -> (b -> c) -> a -> c
(!>>) f g = \a -> g $! f a
{-# INLINE (!>>) #-}

infixr 1 !>>

(<<!) :: (b -> c) -> (a -> b) -> a -> c
(<<!) f g = \a -> f $! g a
{-# INLINE (<<!) #-}

infixr 1 <<!
