module Oat.Span where

data Pos = Pos
  { line :: !Int,
    col :: !Int
  }
  deriving (Show, Eq, Ord)

data Span = Span
  { posStart :: !Pos,
    posEnd :: !Pos
  }
  deriving (Show, Eq, Ord)

class Spanned a where
  spanOf :: a -> Span

instance Spanned Span where
  spanOf = id

-- do some validation here
mkSpan :: Pos -> Pos -> Span
mkSpan = Span