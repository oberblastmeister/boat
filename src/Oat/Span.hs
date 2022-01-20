{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Span
  ( Pos (..),
    Span,
    Spanned (..),
    mkSpan,
    unsafeMkSpan,
  )
where

import Optics

data Pos = Pos
  { line :: !Int,
    col :: !Int
  }
  deriving (Show, Eq, Ord)

data Span = Span
  { start :: !Pos,
    end :: !Pos
  }
  deriving (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''Pos
makeFieldLabelsWith
  ( noPrefixFieldLabels
      & generateUpdateableOptics .~ False
  )
  ''Span

class Spanned a where
  spanOf :: a -> Span

instance Spanned Span where
  spanOf = id

-- do some validation here
mkSpan :: Pos -> Pos -> Span
mkSpan = Span

unsafeMkSpan :: Pos -> Pos -> Span
unsafeMkSpan = Span