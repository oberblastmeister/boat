{-# LANGUAGE QualifiedDo #-}

module Control.Fuel
  ( Fuel (..),
    hasFuel,
    useFuel,
  )
where

import Control.OnLeft (OnLeft (OnLeft))
import Control.OnLeft qualified as OnLeft
import Effectful.Error.Static

data Fuel :: Effect where
  HasFuel :: Fuel m Bool
  UseFuel :: Fuel m ()

type instance DispatchOf Fuel = 'Dynamic

hasFuel :: Fuel :> es => Eff es Bool
hasFuel = send HasFuel

useFuel :: Fuel :> es => Eff es ()
useFuel = send UseFuel