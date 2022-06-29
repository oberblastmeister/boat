module Boat.Dataflow.Label where

import Data.Infinite qualified as Infinite
import Boat.Utils.Source (Source)
import Boat.Utils.Source qualified as Source

newtype Label = Label {unLabel :: Int}
  deriving (Eq, Ord)

instance Show Label where
  show (Label n) = "L" ++ show n

type LabelSource = Source Label

runLabelSource :: Eff (LabelSource ': es) a -> Eff es a
runLabelSource = Source.evalSource $ Label <$> Infinite.from 0
