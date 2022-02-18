{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Fuel
  ( Fuel (..),
    hasFuel,
    useFuel,
  )
where

import Prelude

data Fuel :: Effect where
  HasFuel :: Fuel m Bool
  UseFuel :: Fuel m ()

type instance DispatchOf Fuel = 'Dynamic

hasFuel :: Fuel :> es => Eff es Bool
hasFuel = send HasFuel

useFuel :: Fuel :> es => Eff es ()
useFuel = send UseFuel
