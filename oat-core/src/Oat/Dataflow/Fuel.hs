{-# LANGUAGE TemplateHaskell #-}

module Oat.Dataflow.Fuel
  ( Fuel (..),
    hasFuel,
    useFuel,
    runFuel,
    runFuelInfinite,
  )
where

import Effectful.State.Static.Local qualified as State

data Fuel :: Effect where
  HasFuel :: Fuel m Bool
  UseFuel :: Fuel m ()

$(makeEffect ''Fuel)

validateFuel :: Int -> ()
validateFuel fuel
  | fuel < 0 = error "fuel cannot be less than 0"
  | otherwise = ()

runFuel :: Int -> Eff (Fuel ': es) a -> Eff es a
runFuel fuel = reinterpret (State.evalState fuel) $ \_ -> \case
  HasFuel -> State.gets @Int (> 0)
  UseFuel -> State.modify @Int (- 1)
  where
    !_ = validateFuel fuel

runFuelInfinite :: Eff (Fuel ': es) a -> Eff es a
runFuelInfinite = interpret $ \_ -> \case
  HasFuel -> pure True
  UseFuel -> pure ()
