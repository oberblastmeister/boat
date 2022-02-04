{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Source
  ( Source,
    fresh,
    runSource,
  )
where

import Data.Infinite (Infinite ((::>)))
import Effectful (Eff, type (:>))
import Effectful qualified
import Effectful.Dispatch.Static

data Source :: Type -> Effectful.Effect

type instance Effectful.DispatchOf (Source a) = 'Effectful.Static

newtype instance StaticRep (Source a) = Source (Infinite a)

fresh :: (Source a :> es) => Eff es a
fresh = do
  Source inf <- getStaticRep
  let a ::> as = inf
  putStaticRep $ Source $ as
  pure a

runSource :: Infinite a -> Eff (Source a ': es) a -> Eff es a
runSource infinite = evalStaticRep (Source infinite)