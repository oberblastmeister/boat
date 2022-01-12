module Oat.Common where

import Oat.Interned (unintern)
import Oat.Interned.Text
import Prettyprinter (Doc, Pretty (pretty))

prettyIText :: IText -> Doc ann
prettyIText = pretty . unintern