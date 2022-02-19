module Oat.Error
  ( CompileFail,
    ErrorCode,
    compileFail,
  )
where

import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error

data CompileFail = CompileFail
  deriving (Show, Eq)

data ErrorCode
  = NotFound
  deriving (Show, Eq, Ord, Enum, Bounded)

-- terminate compilation early
compileFail :: forall es a. Error CompileFail :> es => Eff es a
compileFail = Error.throwError CompileFail
