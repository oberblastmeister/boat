{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Oat.Dataflow.LabelSet where

import Control.DeepSeq (NFData)
import Data.IntSet qualified as IntSet
import GHC.Exts (IsList (Item))
import Oat.Dataflow.Label (Label (..))
import Oat.TH (addUnderscoreLenses)
import Oat.Utils.Misc ((#.), (.#))

newtype LabelSet = LabelSet {unLabelSet :: IntSet}
  deriving (Show)
  deriving
    ( Eq,
      Ord,
      Semigroup,
      Monoid,
      NFData
    )
    via IntSet

$(makeLensesWith addUnderscoreLenses ''LabelSet)

type instance IxValue LabelSet = ()

type instance Index LabelSet = Label

instance Ixed LabelSet

instance At LabelSet where
  at i = _unLabelSet % at (i.unLabel)

instance AsEmpty LabelSet

instance IsList (LabelSet) where
  type Item LabelSet = Label
  fromList = LabelSet #. IntSet.fromList .# (fmap (.unLabel))
  toList = fmap Label #. IntSet.toList .# (.unLabelSet)

member :: Label -> LabelSet -> Bool
member label = IntSet.member label.unLabel .# (.unLabelSet)

singleton :: Label -> LabelSet
singleton = LabelSet #. IntSet.singleton .# (.unLabel)
