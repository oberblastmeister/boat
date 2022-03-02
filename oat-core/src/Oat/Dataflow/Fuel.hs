{-# LANGUAGE TemplateHaskell #-}

module Oat.Dataflow.Fuel
  ( Fuel (..),
    getFuel,
    setFuel,
    -- runFuel,
    runFuelInfinite,
    withFuel,
    useFuel,
  )
where

import Control.Applicative (Const (..))
import Control.Monad.State
import Data.Bool (bool)
import Data.Char qualified as Char
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (..))
import Effectful.State.Static.Local (get, put)
import Effectful.State.Static.Local qualified as State
import Prelude hiding (Product)

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

-- runFuel :: Int -> Eff (Fuel ': es) a -> Eff es a
-- runFuel fuel = reinterpret (State.evalState fuel) $ \_ -> \case
--   SetFuel i -> let !_ = validateFuel in put @Int i
--   GetFuel -> get @Int
--   where
--     !_ = validateFuel fuel

runFuelInfinite :: Eff (Fuel ': es) a -> Eff es a
runFuelInfinite = interpret $ \_ -> \case
  SetFuel _ -> pure ()
  GetFuel -> pure maxBound

type Count = Const (Sum Int)

count :: a -> Count b
count _ = Const 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody

test :: Bool -> Int
test = bool 1 0

lciBody :: Char -> Count a
lciBody c = coerce $ test (c == '\n')

lci :: String -> Count [a]
lci = traverse lciBody

wciBody :: Char -> (Compose (State Bool) Count) a
wciBody c = Compose $ state $ updateState c
  where
    updateState :: Char -> Bool -> (Count a, Bool)
    updateState c w = let s = not $ Char.isSpace c in (coerce $ test $ not w && s, s)

wci :: String -> (Compose (State Bool) Count) [a]
wci = traverse wciBody

runWci :: String -> Int
runWci s = coerce $ evalState (getCompose $ wci s) False

clci :: String -> (Product Count Count) [a]
clci = traverse (\c -> Pair (cciBody c) (lciBody c))

