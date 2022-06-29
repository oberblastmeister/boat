{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Pos (Pos (..)) where

data Pos = Pos
  { line :: !Int,
    col :: !Int,
    pos :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

$(makeFieldLabelsNoPrefix ''Pos)
