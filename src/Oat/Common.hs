module Oat.Common where

import Data.Interned (unintern)
import Data.Interned.Internal.Text (InternedText)
import Prettyprinter (Doc, Pretty (pretty))

prettyIText :: InternedText -> Doc ann
prettyIText = pretty . unintern