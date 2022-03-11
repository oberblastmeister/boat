module Oat.LL.Pass.Manager where

import Data.Foldable (foldlM)
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async qualified as Async
import Oat.LL.Ir qualified as Ir

data PassTag
  = DeadCode
  | ConstProp
  deriving (Show, Eq)

type BodyTrans es = Ir.FunBody -> Eff es Ir.FunBody

type ModuleTrans es = [Ir.FunBody] -> Eff es [Ir.FunBody]

data ModulePass es = BodyPass
  { tag :: !PassTag,
    bodyTrans :: (BodyTrans es)
  }

runPasses :: Concurrent :> es => [ModulePass es] -> ModuleTrans es
runPasses passes mod = foldlM go mod passes
  where
    go !mod (BodyPass {tag, bodyTrans}) = Async.pooledMapConcurrently bodyTrans mod
