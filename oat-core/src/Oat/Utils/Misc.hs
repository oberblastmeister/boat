module Oat.Utils.Misc 
  ( inBetween,
    pathTail,
    concatToEither,
    maybeToRight,
    maybeToLeft,
    rightToMaybe,
    leftToMaybe,
    mVecFromList,
    timSortListBy,
    timSortNEBy,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Range (Range (RangeP))
import Data.Vector qualified as VB
import Data.Vector.Algorithms.Tim qualified as Vector.Algorithms.Tim
import Data.Vector.Mutable qualified as VBM
import System.FilePath qualified as FilePath
import Prelude hiding (Map, imap)

-- non but only a lens

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

timSortListBy :: (a -> a -> Ordering) -> [a] -> [a]
timSortListBy cmp as = VB.toList $
  VB.create $ do
    mv <- mVecFromList as
    Vector.Algorithms.Tim.sortBy cmp mv
    pure mv

timSortNEBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
timSortNEBy cmp = NonEmpty.fromList . timSortListBy cmp . NonEmpty.toList

