module Oat.Common
  ( (++>),
    pattern (:>),
    internalError,
    unwrap,
    unreachable,
    hashSetOf,
    makeFieldGetterLabelsNoPrefix,
    inBetween,
  )
where

import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import Data.Range (Range (RangeP))
import qualified Language.Haskell.TH as TH
import Optics hiding ((:>))

infixl 4 :>

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x : xs where xs :> x = x : xs

{-# COMPLETE (:>), [] #-}

(++>) :: [a] -> [a] -> [a]
(++>) l l' = l' ++ l

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