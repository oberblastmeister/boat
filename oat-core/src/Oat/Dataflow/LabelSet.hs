{-# LANGUAGE TemplateHaskell #-}

module Oat.Dataflow.LabelSet where

import Control.DeepSeq (NFData)
import Data.IntSet qualified as IntSet
import GHC.Exts (IsList (Item))
import GHC.Exts qualified
import Oat.Dataflow.Label (Label (..))
import Oat.TH (addUnderscoreLenses)

newtype LabelSet = LabelMap {unLabelSet :: IntSet}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      NFData
    )

$(makeLensesWith addUnderscoreLenses ''LabelSet)

type instance IxValue LabelSet = ()

type instance Index LabelSet = Label

instance Ixed LabelSet

instance At LabelSet where
  at i = _unLabelSet % at (i.unLabel)

instance AsEmpty LabelSet

instance IsList (LabelSet) where
  type Item LabelSet = Label
  fromList = coerce @IntSet @LabelSet . IntSet.fromList . coerce
  toList = coerce @[Int] @[Label] . IntSet.toList . coerce

member :: Label -> LabelSet -> Bool
member label = IntSet.member (coerce label) . coerce
