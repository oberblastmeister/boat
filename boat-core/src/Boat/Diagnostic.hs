{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Boat.Diagnostic where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Range (Range)
import Data.Range qualified as Range
import Data.Span (Span)
import Data.Vector qualified as VB
import Boat.Utils.Misc (timSortNEBy)
import Boat.Error (ErrorCode)
import Boat.Utils.Pretty (Ann)
import Boat.Utils.Optics (onOf)
import Prettyprinter (Doc)

data DiagnosticSeverity
  = DiagnosticError
  | DaignosticWarning
  | DiagnosticInfo

data LineRange = LineRange
  { line :: !Int,
    path :: !FilePath,
    range :: !Range
  }

data Label = Label
  { doc :: forall ann. Doc ann,
    path :: FilePath,
    lineRange :: LineRange
  }

data Note = Note
  { doc :: forall ann. Doc ann
  }

data Diagnostic = Diagnostic
  { msg :: forall ann. Doc ann,
    code :: !ErrorCode,
    level :: !DiagnosticSeverity,
    notes :: [Note],
    span :: !Span,
    help :: forall ann. Doc ann
  }

type Source = VB.Vector Text

type SourceMap = HashMap FilePath Source

$(makeFieldLabelsNoPrefix ''LineRange)
$(makeFieldLabelsNoPrefix ''Label)
$(makeFieldLabelsNoPrefix ''Diagnostic)

instance LabelOptic "range" A_Lens Label Label Range Range where
  labelOptic = #lineRange % #range

prettyDiagnostic :: SourceMap -> Diagnostic -> Doc Ann
prettyDiagnostic sourceMap diagnostic = undefined
  where
    -- grouped = do
    --   byFile <-
    --     diagnostic ^. #labels
    --       & NonEmpty.groupBy ((==) `onOf` #path)
    --   [byFile ^. _neHead % lineO & NonEmpty.groupBy ((==) `onOf` lineO)]

    -- groupedLabels = do
    --   NonEmpty.groupBy
    --     ((==) `onOf` lineO)
    --     (diagnostic ^. #labels)
    --     & timSortListBy (compare `onOf` _neHead % lineO)
    --     & map (\labels -> (labels, getCombinedRanges labels))

    -- lineO = #lineRange % #line
    -- groupedLabels = VB.create $ do
    --   let lineO = #lineRange % #line
    --   mv <-
    --       & mVecFromList
    --   Vector.Algorithms.Tim.sortBy (compare `onOf` _1 % _neHead % lineO) mv
    --   pure mv

    getCombinedRanges :: NonEmpty Label -> NonEmpty Range
    getCombinedRanges labels =
      -- this should never panic
      NonEmpty.fromList combined
      where
        combined =
          foldr
            ( \range1 -> \case
                [] -> [range1]
                (range2 : ranges) -> combine range1 range2 ++ ranges
            )
            []
            (timSortNEBy (compare `onOf` #start) (view #range <$> labels))
        combine range1 range2 = case range1 `Range.intersect` range2 of
          Just _ -> [range1 `Range.cover` range2]
          Nothing -> [range1, range2]

-- sorted = VB.create $ do
--   mv <- mVecFromList $ NonEmpty.toList labels
--   Vector.Algorithms.Tim.sortBy (compare `onOf` #lineRange % #range % #start) mv
--   pure mv

-- rangeKey = #end

-- key = (^. #lineRange % #line)
