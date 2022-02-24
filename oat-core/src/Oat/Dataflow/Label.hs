module Oat.Dataflow.Label where

import Control.Source (Source)
import Control.Source qualified as Source
import Data.Infinite qualified as Infinite

newtype Label = Label {unLabel :: Int}
  deriving (Eq, Ord)

instance Show Label where
  show (Label n) = "L" ++ show n

type LabelSource = Source Label

runLabelSource :: Eff (LabelSource ': es) a -> Eff es a
runLabelSource = Source.evalSource $ Label <$> Infinite.from 0

-- fresh :: LabelSource :> es -> Eff es Label
-- fresh = Source.fresh
