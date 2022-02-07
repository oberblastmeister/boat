module Oat.TH
  ( getterFieldLabels,
    addUnderscoreLenses,
    addUnderscoreNamer,
  )
where

import Language.Haskell.TH qualified as TH

getterFieldLabels :: LensRules
getterFieldLabels =
  noPrefixFieldLabels
    & generateUpdateableOptics .~ False

addUnderscoreLenses :: LensRules
addUnderscoreLenses = lensRules & lensField .~ addUnderscoreNamer

addUnderscoreNamer :: FieldNamer
addUnderscoreNamer = mappingNamer $ pure . ('_' :)