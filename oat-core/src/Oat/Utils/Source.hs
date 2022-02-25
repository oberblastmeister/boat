module Oat.Utils.Source
  ( Source,
    IdSource,
    fresh,
    runSource,
    evalSource,
    runIdSource,
  )
where

import Data.Infinite (Infinite ((::>)))
import Data.Infinite qualified as Infinite

data Source :: Type -> Effect

type instance DispatchOf (Source a) = 'Static 'NoSideEffects

newtype instance StaticRep (Source a) = Source (Infinite a)

fresh :: Source a :> es => Eff es a
fresh = do
  Source inf <- getStaticRep
  let a ::> as = inf
  putStaticRep $ Source as
  pure a

runSource :: Infinite a -> Eff (Source a ': es) b -> Eff es (b, StaticRep (Source a))
runSource infinite = runStaticRep (Source infinite)

evalSource :: Infinite a -> Eff (Source a ': es) b -> Eff es b
evalSource infinite = evalStaticRep (Source infinite)

type IdSource = Source Int

runIdSource :: Eff (IdSource ': es) a -> Eff es a
runIdSource = evalSource $ Infinite.from 0
