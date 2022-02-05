{-# LANGUAGE QuantifiedConstraints #-}

module Oat.Common
  ( internalError,
    unwrap,
    unreachable,
    hashSetOf,
    makeFieldGetterLabelsNoPrefix,
    inBetween,
    insOrdSetOf,
    swap,
  )
where

import Data.HashSet qualified as HashSet
import Data.HashSet.InsOrd (InsOrdHashSet)
import Data.HashSet.InsOrd qualified as InsOrdHashSet
import Data.IntMap qualified as IntMap
import Data.Range (Range (RangeP))
import Language.Haskell.TH qualified as TH

internalError :: forall a. HasCallStack => Text -> a
internalError t = error $ "Internal compiler error: " <> t

-- non but only a lens
unwrap :: a -> Lens' (Maybe a) a
unwrap def =
  lens
    (fromMaybe def)
    ( \m b -> case m of
        Just _ -> Just b
        Nothing -> Just def
    )

unreachable :: forall a. a
unreachable = error "Unreachable!"

hashSetOf :: (Eq a, Hashable a, Is k A_Fold) => Optic' k is s a -> s -> HashSet a
hashSetOf fold = foldMapOf fold HashSet.singleton

makeFieldGetterLabelsNoPrefix :: TH.Name -> TH.DecsQ
makeFieldGetterLabelsNoPrefix =
  makeFieldLabelsWith $
    noPrefixFieldLabels
      & generateUpdateableOptics .~ False

inBetween :: Range -> IntMap a -> IntMap a
inBetween (RangeP start end) imap = imap''
  where
    (imap'', _) = IntMap.split end imap'
    (_, imap') = IntMap.split start imap

insOrdSetOf :: (Is k A_Fold, Eq a, Hashable a) => Optic' k is s a -> s -> InsOrdHashSet a
insOrdSetOf o = foldlOf' o (flip InsOrdHashSet.insert) mempty

swap :: (Is k An_AffineFold, Is k' A_Review) => Optic' k is t a -> Optic' k' is' t b -> Setter t t a b
swap o o' = sets $ \f x -> case x ^? o of
  Nothing -> x
  Just y -> o' # f y