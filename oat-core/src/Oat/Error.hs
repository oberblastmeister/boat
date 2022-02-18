module Oat.Error
  ( CompileFail,
    reportFail,
    ErrorCode,
    compileFail,
    runReportFail,
  )
where

import Effectful.Error.Static
import Oat.Reporter

data CompileFail = CompileFail
  deriving (Show, Eq)

reportFail :: forall w es a. '[Reporter w, Error CompileFail] :>> es => w -> Eff es a
reportFail w = report w *> throwError CompileFail

runReportFail :: Eff (Reporter [a] : Error CompileFail : es) b -> Eff es (Either (CallStack, CompileFail) (b, [a]))
runReportFail = runError @CompileFail . runReporterList

data ErrorCode
  = NotFound
  deriving (Show, Eq, Ord, Enum, Bounded)

-- terminate compilation early
compileFail :: forall es a. Error CompileFail :> es => Eff es a
compileFail = throwError CompileFail
