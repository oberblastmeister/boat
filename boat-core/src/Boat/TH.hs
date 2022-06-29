module Boat.TH
  ( getterFieldLabels,
    addUnderscoreLenses,
    addUnderscoreNamer,
    makeFieldLabelsForOnly,
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

makeFieldLabelsForOnly :: [String] -> TH.Name -> TH.DecsQ
makeFieldLabelsForOnly names = makeFieldLabelsFor $ zip names names
