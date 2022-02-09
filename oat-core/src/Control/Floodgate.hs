module Control.Floodgate where

import Effectful.Error.Static
import Unsafe.Coerce qualified

data Floodgate :: Effect

type instance DispatchOf Floodgate = 'Static

newtype instance StaticRep Floodgate = Floodgate [Any]

type NeedsIO :: Effect -> Bool
type family NeedsIO e

hold :: forall es. Floodgate :> es => Eff es () -> Eff es ()
hold m = do
  Floodgate ms <- getStaticRep
  putStaticRep $ Floodgate $ Unsafe.Coerce.unsafeCoerce @(Eff es ()) @Any m : ms

release :: forall es. Floodgate :> es => Eff es ()
release = do
  Floodgate ms <- getStaticRep
  putStaticRep $ Floodgate []
  sequence_ $ reverse $ Unsafe.Coerce.unsafeCoerce @[Any] @[Eff es ()] ms

releaseReverse :: forall es. Floodgate :> es => Eff es ()
releaseReverse = do
  Floodgate ms <- getStaticRep
  putStaticRep $ Floodgate []
  sequence_ $ Unsafe.Coerce.unsafeCoerce @[Any] @[Eff es ()] ms

runFloodgate :: Eff (Floodgate : es) a -> Eff es a
runFloodgate = evalStaticRep $ Floodgate []
