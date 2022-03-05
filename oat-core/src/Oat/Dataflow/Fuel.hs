{-# LANGUAGE TemplateHaskell #-}

module Oat.Dataflow.Fuel
  ( Fuel (..),
    getFuel,
    setFuel,
    hasFuel,
    modifyFuel,
    runFuel,
    runFuelInfinite,
    withFuel,
    useFuel,
  )
where

import Effectful.State.Static.Local (get, gets, put)
import Effectful.State.Static.Local qualified as State
import Prelude hiding (Product)

data Fuel :: Effect where
  SetFuel :: Int -> Fuel m ()
  GetFuel :: Fuel m Int
  HasFuel :: Fuel m Bool

$(makeEffect ''Fuel)

modifyFuel :: Fuel :> es => (Int -> Int) -> Eff es ()
modifyFuel f = getFuel >>= setFuel . f

useFuel :: Fuel :> es => Eff es ()
useFuel = modifyFuel $ max 0

withFuel :: Fuel :> es => a -> Eff es (Maybe a)
withFuel a = do
  fuel <- getFuel
  if fuel == 0
    then pure Nothing
    else do setFuel $ fuel - 1; pure $ Just a

validateFuel :: HasCallStack => Int -> ()
validateFuel fuel
  | fuel < 0 = error "fuel cannot be less than 0"
  | otherwise = ()

runFuel :: Int -> Eff (Fuel ': es) a -> Eff es a
runFuel fuel = reinterpret (State.evalState fuel) $ \_ -> \case
  SetFuel i -> let !_ = validateFuel in put @Int i
  GetFuel -> get @Int
  HasFuel -> gets @Int (> 0)
  where
    !_ = validateFuel fuel

runFuelInfinite :: Eff (Fuel ': es) a -> Eff es a
runFuelInfinite = interpret $ \_ -> \case
  SetFuel fuel -> let !_ = validateFuel fuel in pure ()
  GetFuel -> pure maxBound
  HasFuel -> pure True
