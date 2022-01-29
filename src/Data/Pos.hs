{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Pos (Pos (..)) where

import Optics

data Pos = Pos
  { line :: !Int,
    col :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

makeFieldLabelsNoPrefix ''Pos