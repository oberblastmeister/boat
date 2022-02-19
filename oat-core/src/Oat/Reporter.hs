{-# LANGUAGE AllowAmbiguousTypes #-}

module Oat.Reporter
  ( Reporter (..),
    report,
    runReporterList,
    hadFatal,
    reportFail,
    maybeFail,
  )
where

import Effectful.Error.Static (Error)
import Effectful.Writer.Static.Shared qualified as Writer
import Oat.Error (compileFail)
import Oat.Error qualified
import Oat.Utils.Monad (whenM)

data Reporter :: Type -> Effect where
  Report :: w -> Reporter w m ()
  HadFatal :: Reporter w m Bool

type instance DispatchOf (Reporter w) = 'Dynamic

report :: forall w es. Reporter w :> es => w -> Eff es ()
report = send . Report

hadFatal :: forall w es. Reporter w :> es => Eff es Bool
hadFatal = send $ HadFatal @w

maybeFail :: forall w es. '[Reporter w, Error Oat.Error.CompileFail] :>> es => Eff es ()
maybeFail = whenM (hadFatal @w) compileFail

reportFail :: forall w es a. '[Reporter w, Error Oat.Error.CompileFail] :>> es => w -> Eff es a
reportFail w = report w *> compileFail

runReporterList :: forall a es b. Eff (Reporter [a] ': es) b -> Eff es (b, [a])
runReporterList m = do
  (b, Dual as) <-
    reinterpret
      (Writer.runWriter @(Dual [a]))
      ( \_ -> \case
          Report w -> Writer.tell $ Dual w
          HadFatal -> pure False
      )
      m
  pure (b, as)