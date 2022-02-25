{-# LANGUAGE TemplateHaskell #-}

module Oat.Dataflow.Fuel
  ( Fuel (..),
    getFuel,
    setFuel,
    runFuel,
    runFuelInfinite,
    withFuel,
    useFuel,
  )
where

import Effectful.State.Static.Local (get, put)
import Effectful.State.Static.Local qualified as State

data Fuel :: Effect where
  SetFuel :: Int -> Fuel m ()
  GetFuel :: Fuel m Int

$(makeEffect ''Fuel)

useFuel :: Fuel :> es => Eff es ()
useFuel = getFuel >>= (setFuel . max 0)

withFuel :: Fuel :> es => a -> Eff es (Maybe a)
withFuel a = do
  fuel <- getFuel
  if fuel == 0
    then pure Nothing
    else do setFuel $ fuel - 1; pure $ Just a

validateFuel :: Int -> ()
validateFuel fuel
  | fuel < 0 = error "fuel cannot be less than 0"
  | otherwise = ()

runFuel :: Int -> Eff (Fuel ': es) a -> Eff es a
runFuel fuel = reinterpret (State.evalState fuel) $ \_ -> \case
  SetFuel i -> let !_ = validateFuel in put @Int i
  GetFuel -> get @Int
  where
    !_ = validateFuel fuel

runFuelInfinite :: Eff (Fuel ': es) a -> Eff es a
runFuelInfinite = interpret $ \_ -> \case
  SetFuel _ -> pure ()
  GetFuel -> pure maxBound
