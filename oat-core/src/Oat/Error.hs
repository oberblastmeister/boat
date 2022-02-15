module Oat.Error
  ( CompileFail,
    reportFail,
  )
where

import Effectful.Error.Static
import Oat.Reporter

data CompileFail = CompileFail
  deriving (Show, Eq)

reportFail :: forall w es a. '[Reporter w, Error CompileFail] :>> es => w -> Eff es a
reportFail w = report w *> throwError CompileFail