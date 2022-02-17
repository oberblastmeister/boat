{-# LANGUAGE AllowAmbiguousTypes #-}

module Oat.Reporter
  ( Reporter (..),
    report,
    runReporterCallback,
    runReporterPure,
    runReporterList,
  )
where

import Effectful.Writer.Static.Shared

data Reporter :: Type -> Effect where
  Report :: w -> Reporter w m ()

type instance DispatchOf (Reporter w) = 'Dynamic

report :: forall w es. Reporter w :> es => w -> Eff es ()
report = send . Report

-- runReporterPure specialized to a list so that we don't get O(n^2) complexity
runReporterList :: forall a es b. Eff (Reporter [a] ': es) b -> Eff es (b, [a])
runReporterList m = do
  (b, Dual as) <-
    reinterpret
      (runWriter @(Dual [a]))
      ( \_ -> \case
          Report w -> tell $ Dual w
      )
      m
  pure (b, as)

runReporterPure :: forall w es a. Monoid w => Eff (Reporter w ': es) a -> Eff es (a, w)
runReporterPure = reinterpret (runWriter @w) $ \_ -> \case
  Report w -> tell w

runReporterCallback :: forall w es a. (w -> Eff es ()) -> Eff (Reporter w ': es) a -> Eff es a
runReporterCallback f = interpret $ \_ -> \case
  Report w -> f w
