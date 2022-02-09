module Control.Source
  ( Source,
    fresh,
    runSource,
    evalSource,
  )
where

import Data.Infinite (Infinite ((::>)))
import Effectful.Error.Static

data Source :: Type -> Effect

type instance DispatchOf (Source a) = 'Static

newtype instance StaticRep (Source a) = Source (Infinite a)

fresh :: Source a :> es => Eff es a
fresh = do
  Source inf <- getStaticRep
  let a ::> as = inf
  putStaticRep $ Source as
  pure a

runSource :: Infinite a -> Eff (Source a : es) b -> Eff es (b, StaticRep (Source a))
runSource infinite = runStaticRep (Source infinite)

evalSource :: Infinite a -> Eff (Source a : es) b -> Eff es b
evalSource infinite = evalStaticRep (Source infinite)
