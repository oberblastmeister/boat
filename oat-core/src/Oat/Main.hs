module Oat.Main
  ( main,
    mainWithOpt,
    mainWithOpt_,
    runMain,
    runMain_,
    runDriverMain,
    runDriverMain_,
  )
where

import Effectful.Error.Static (CallStack, Error, prettyCallStack, runError)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static.Optics (rview)
import Oat.Driver qualified as Driver
import Oat.Error (CompileFail)
import Oat.Opt (Opt)
import Oat.Opt qualified as Opt
import Oat.RunReporter qualified as RunReporter
import Oat.Utils.Families (type (++))
import Oat.Utils.Monad (whenJust, whenM)
import Oat.Utils.Pretty (Ann (Info), pError, pIf, pWarning, (<#>), (<++>))
import Oat.Utils.Pretty qualified as Utils.Pretty
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty
import System.Exit qualified as Exit
import UnliftIO.Exception qualified as Exception

main :: IO ()
main =
  Opt.opt >>= mainWithOpt >>= \case
    True -> liftIO Exit.exitSuccess
    False -> liftIO Exit.exitFailure

mainWithOpt_ :: Opt -> IO ()
mainWithOpt_ opt = runMain_ opt $ Driver.runDriver Driver.drive

mainWithOpt :: Opt -> IO Bool
mainWithOpt opt = runMain opt $ Driver.runDriver Driver.drive

type MainEffs =
  '[Error CompileFail]
    ++ RunReporter.ReporterEffs
    ++ '[Reader Opt, IOE]

runMain_ :: (es' ~ MainEffs) => Opt -> Eff es' () -> IO ()
runMain_ opt m =
  runMain opt m >>= \case
    True -> pure ()
    False -> Exception.throwString "Throwing exception due to compilation failure"

runMain :: (es' ~ MainEffs) => Opt -> Eff es' () -> IO Bool
runMain opt m = do
  runEff $
    runReader opt $
      RunReporter.runAllReporters
        (runError @CompileFail m)
        >>= \case
          (Left (callStack, _), reports) -> do
            printCompileResult (Just callStack) reports
            pure False
          (Right (), reports) -> do
            if reports ^. #errors > 0
              then do
                printCompileResult Nothing reports
                pure False
              else pure True

runDriverMain_ :: (es' ~ Driver.DriverEffsRun ++ MainEffs) => Opt -> Eff es' () -> IO ()
runDriverMain_ opt m = runMain_ opt $ Driver.runDriver m

runDriverMain :: (es' ~ Driver.DriverEffsRun ++ MainEffs) => Opt -> Eff es' () -> IO Bool
runDriverMain opt m = runMain opt $ Driver.runDriver m

printCompileResult :: '[IOE, Reader Opt] :>> es => Maybe CallStack -> RunReporter.Reports -> Eff es ()
printCompileResult callStack reports = do
  Utils.Pretty.putDoc $
    pIf
      (reports ^. #warnings > 0)
      ( pWarning
          <> ": "
          <> pretty (reports ^. #warnings)
          <> " warnings generated"
      )
      <++> pError
      <> ": Compilation failed due to "
      <> pretty (reports ^. #errors)
      <+> ( if reports ^. #errors == 1
              then "error"
              else "errors"
          )
      <> Pretty.hardline
  whenJust callStack $ \callStack ->
    whenM (rview @Opt #callStack) $
      Utils.Pretty.putDoc $
        Info <#> "Callstack:"
          <++> pretty (prettyCallStack callStack)
