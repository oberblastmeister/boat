{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.RunReporter where

import Effectful.State.Static.Local
import Oat.Common (type (++))
import Oat.Driver qualified as Driver
import Oat.LL qualified as LL
import Oat.Reporter (Reporter (Report))
import Oat.Reporter qualified as Reporter
import Effectful.State.Static.Local.Optics (modifying)

type ReporterEffs =
  '[ Reporter [Driver.DriverError],
     Reporter [LL.ParseError],
     Reporter [LL.CheckError]
   ]

data Reports = Reports {errors :: !Int, warnings :: !Int}

$(makeFieldLabelsNoPrefix ''Reports)

instance Semigroup Reports where
  Reports {errors, warnings} <> Reports {errors = errors', warnings = warnings'} =
    Reports
      { errors = errors + errors',
        warnings = warnings + warnings'
      }

instance Monoid Reports where
  mempty = Reports {errors = 0, warnings = 0}
  mappend = (<>)

runAllReporters ::
  '[IOE] :>> es =>
  Eff (ReporterEffs ++ es) a ->
  Eff es (a, Reports)
runAllReporters m = do
  (((a, r1), r2), r3) <-
    run @Driver.DriverError m
      & run @LL.ParseError
      & run @LL.CheckError
  pure (a, r1 <> r2 <> r3)
  where
    run ::
      forall x f es a.
      ( IOE :> es,
        Show x,
        Foldable f
      ) =>
      Eff (Reporter (f x) ': es) a ->
      Eff es (a, Reports)
    run =
      runReporter
        ( \x -> do
            liftIO $ print x
            modifying @Reports #errors (+ 1)
        )

runReporter :: (Foldable f) => (x -> Eff (State Reports ': es) ()) -> Eff (Reporter (f x) ': es) a -> Eff es (a, Reports)
runReporter report = reinterpret (runState mempty) $ \_ -> \case
  Report w -> traverse_ report w

runReporterCallBackEach ::
  forall f x es a.
  Foldable f =>
  (x -> Eff es ()) ->
  Eff (Reporter (f x) ': es) a ->
  Eff es a
runReporterCallBackEach f = Reporter.runReporterCallback (traverse_ f)
