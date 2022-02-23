module Oat.Optimize.Label where

newtype Label = Label {unLabel :: Int}
  deriving (Eq, Ord)

instance Show Label where
  show (Label n) = "L" ++ show n