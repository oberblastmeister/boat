module Control.Fuel
  ( Fuel (..),
    hasFuel,
    useFuel,
  )
where

import Control.FlattenEither qualified as E
import Effectful.Error.Static

data Fuel :: Effect where
  HasFuel :: Fuel m Bool
  UseFuel :: Fuel m ()

type instance DispatchOf Fuel = 'Dynamic

hasFuel :: Fuel :> es => Eff es Bool
hasFuel = send HasFuel

useFuel :: Fuel :> es => Eff es ()
useFuel = send UseFuel

runSomething :: Eff es ()
runSomething = do
  res <-
    runError @Int something
      & runError @Bool
      & runError @Char
  let another = E.do

  let final = case res of
        Left e -> Left $ show e
        Right rest -> case rest of
          Left e -> Left $ show e
          Right rest -> case rest of
            Left e -> Left $ show e
            Right a -> Right a
  pure ()

something :: '[Error Int, Error Bool, Error Char] :>> es => Eff es ()
something = pure ()