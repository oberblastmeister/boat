{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Fuel
  ( Fuel (..),
    hasFuel,
    useFuel,
  )
where

import Effectful.Dispatch.Static (unsafeEff_)

data Fuel :: Effect where
  HasFuel :: Fuel m Bool
  UseFuel :: Fuel m ()

type instance DispatchOf Fuel = 'Dynamic

hasFuel :: Fuel :> es => Eff es Bool
hasFuel = send HasFuel

useFuel :: Fuel :> es => Eff es ()
useFuel = send UseFuel

data FileSystem :: Effect

type instance DispatchOf FileSystem = 'Static

type NeedsIO :: Effect -> Bool
type family NeedsIO e

type instance NeedsIO FileSystem = 'True

deferIO :: (DispatchOf e ~ 'Static, NeedsIO e ~ 'True, e :> es) => IO a -> Eff es a
deferIO = unsafeEff_

evalStaticRepDeferred :: (DispatchOf e ~ 'Static, NeedsIO e ~ 'True, IOE :> es) => StaticRep e -> Eff (e ': es) a -> Eff es a
evalStaticRepDeferred = evalStaticRep

testing :: Eff '[FileSystem] ()
testing = deferIO @FileSystem $ do
  putStrLn "hello world!"
