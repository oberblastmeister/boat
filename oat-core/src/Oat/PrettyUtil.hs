module Oat.PrettyUtil
  ( ann,
    Ann(..),
    reAnnotateAnsi,
    putDoc,
  )
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

ann :: ann -> Doc ann -> Doc ann
ann = Pretty.annotate

data Ann
  = Error
  | Warning
  | Info

reAnnotateAnsi :: Ann -> AnsiStyle
reAnnotateAnsi Error = color Red <> bold
reAnnotateAnsi Warning = color Yellow <> bold
reAnnotateAnsi Info = color Blue <> bold

putDoc :: IOE :> es => Doc Ann -> Eff es ()
putDoc doc = liftIO $ Pretty.Render.Terminal.renderIO IO.stdout docStream'
  where
    docStream' = Pretty.reAnnotateS reAnnotateAnsi docStream
    docStream = Pretty.layoutPretty Pretty.defaultLayoutOptions doc
