module Oat.TH
  ( getterFieldLabels,
    addUnderscoreLenses,
    addUnderscoreNamer,
  )
where

getterFieldLabels :: LensRules
getterFieldLabels =
  noPrefixFieldLabels
    & generateUpdateableOptics .~ False

addUnderscoreLenses :: LensRules
addUnderscoreLenses = lensRules & lensField .~ addUnderscoreNamer

addUnderscoreNamer :: FieldNamer
addUnderscoreNamer = mappingNamer $ pure . ('_' :)
