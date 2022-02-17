module Oat.PrettyUtil
  ( Ann (..),
    reAnnotateAnsi,
    putDoc,
    pInfo,
    pWarning,
    pError,
    (<#>),
  pIf)
where

import Prettyprinter (Doc)
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Terminal
  ( AnsiStyle,
    Color (..),
    bold,
    color,
  )
import Prettyprinter.Render.Terminal qualified as Pretty.Render.Terminal
import System.IO qualified as IO

data Ann
  = Error
  | Warning
  | Info

reAnnotateAnsi :: Ann -> AnsiStyle
reAnnotateAnsi Error = color Red <> bold
reAnnotateAnsi Warning = color Yellow <> bold
reAnnotateAnsi Info = color Cyan <> bold

pIf :: Bool -> Doc ann -> Doc ann
pIf cond doc
  | cond = doc
  | otherwise = mempty

putDoc :: IOE :> es => Doc Ann -> Eff es ()
putDoc doc = liftIO $ Pretty.Render.Terminal.renderIO IO.stdout docStream'
  where
    docStream' = Pretty.reAnnotateS reAnnotateAnsi docStream
    docStream = Pretty.layoutPretty Pretty.defaultLayoutOptions doc

pError :: Doc Ann
pError = Error <#> "error"

pWarning :: Doc Ann
pWarning = Warning <#> "warning"

pInfo :: Doc Ann
pInfo = Info <#> "info"

(<#>) :: Ann -> Doc Ann -> Doc Ann
ann <#> doc = Pretty.annotate ann doc

infixr 9 <#>
