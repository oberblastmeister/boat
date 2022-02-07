{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Span
  ( Span (SpanP, SpanP'),
    new,
    unsafeNew,
    new',
    contains,
    empty,
    empty',
  )
where

import Data.Pos (Pos (..))
import Oat.TH (getterFieldLabels)
import Prelude hiding (contains, empty)

data Span = Span
  { start :: !Pos,
    end :: !Pos
  }
  deriving (Show, Eq, Ord)

$(makeFieldLabelsWith getterFieldLabels ''Span)

unsafeNew :: Pos -> Pos -> Span
unsafeNew = Span

new :: Pos -> Pos -> Span
new start@Pos {line, col} end@Pos {line = line', col = col'} =
  let !_ = line > line' && error "Data.Span.new: start line must be less than or equal to end line"
      !_ = line /= line' || col > col' && error "Data.Span.new: start col must be less than end col when line is the same"
   in Span {start, end}

new' :: Int -> Int -> Int -> Int -> Span
new' line col line' col' = new (Pos line col) (Pos line' col')

empty :: Pos -> Span
empty pos = Span {start = pos, end = pos}

empty' :: Int -> Int -> Span
empty' line = empty . Pos line

pattern SpanP :: Pos -> Pos -> Span
pattern SpanP start end <-
  Span {start, end}
  where
    SpanP = new

{-# COMPLETE SpanP #-}

pattern SpanP' :: Int -> Int -> Int -> Int -> Span
pattern SpanP' line1 col1 line2 col2 <-
  Span
    { start = Pos {line = line1, col = col1},
      end = Pos {line = line2, col = col2}
    }

{-# COMPLETE SpanP' #-}

-- | Does the first span contain the other?
contains :: Span -> Span -> Bool
contains (SpanP' line1 col1 line2 col2) (SpanP' line1' col1' line2' col2') =
  line1 <= line1' && line2' <= line2
    && (line1 /= line1' || col1 <= col1')
    && (line2 /= line2' || col2' <= col2)