module Oat.Main
  ( main,
    mainWithArgs,
    mainWithArgs_,
    runMain,
    runMain_,
    runDriverMain,
    runDriverMain_,
  )
where

import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static.Optics (rview)
import Oat.Driver qualified as Driver
import Oat.Error (CompileFail)
import Oat.Cli (Args)
import Oat.Cli qualified as Cli
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
  Cli.args >>= mainWithArgs >>= \case
    True -> liftIO Exit.exitSuccess
    False -> liftIO Exit.exitFailure

mainWithArgs_ :: Args -> IO ()
mainWithArgs_ args = runMain_ args $ Driver.runDriver Driver.drive

mainWithArgs :: Args -> IO Bool
mainWithArgs args = runMain args $ Driver.runDriver Driver.drive

type MainEffs =
  '[Error CompileFail]
    ++ RunReporter.ReporterEffs
    ++ '[Reader Args, IOE]

runMain_ :: (es' ~ MainEffs) => Args -> Eff es' () -> IO ()
runMain_ args m =
  runMain args m >>= \case
    True -> pure ()
    False -> Exception.throwString "Throwing exception due to compilation failure"

runMain :: (es' ~ MainEffs) => Args -> Eff es' () -> IO Bool
runMain args m = do
  runEff $
    runReader args $
      RunReporter.runAllReporters
        (Error.runError @CompileFail m)
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

runDriverMain_ :: (es' ~ Driver.DriverEffsRun ++ MainEffs) => Args -> Eff es' () -> IO ()
runDriverMain_ args m = runMain_ args $ Driver.runDriver m

runDriverMain :: (es' ~ Driver.DriverEffsRun ++ MainEffs) => Args -> Eff es' () -> IO Bool
runDriverMain args m = runMain args $ Driver.runDriver m

printCompileResult :: '[IOE, Reader Args] :>> es => Maybe Error.CallStack -> RunReporter.Reports -> Eff es ()
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
    whenM (rview @Args #callStack) $
      Utils.Pretty.putDoc $
        Info <#> "Callstack:"
          <++> pretty (Error.prettyCallStack callStack)
