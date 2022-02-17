{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Oat.Main where

import UnliftIO.Exception qualified as Exception
import Effectful.Error.Static (CallStack, Error, prettyCallStack, runError)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static.Optics (rview)
import Oat.Common (whenM, type (++))
import Oat.Driver qualified as Driver
import Oat.Error (CompileFail)
import Oat.Opt (Opt)
import Oat.Opt qualified as Opt
import Oat.PrettyUtil (Ann (Info), pError, pIf, pWarning, (<#>))
import Oat.PrettyUtil qualified as PrettyUtil
import Oat.RunReporter qualified as RunReporter
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty
import System.Exit qualified as Exit

main :: IO ()
main = Opt.opt >>= mainWithOpt

mainWithOpt :: Opt -> IO ()
mainWithOpt opt = runMain opt $ Driver.runDriver Driver.drive

type MainEffs =
  '[Error CompileFail]
    ++ RunReporter.ReporterEffs
    ++ '[Reader Opt, IOE]

runMain :: (es' ~ MainEffs) => Opt -> Eff es' () -> IO ()
runMain opt m = do
  runEff $
    runReader opt $
      RunReporter.runAllReporters
        (runError @CompileFail m)
        >>= \case
          (Left (callStack, _), reports) -> do
            printCompileResult callStack reports
            liftIO Exit.exitFailure
          (Right (), reports) -> do
            when (reports ^. #errors > 0) $
              Exception.throwString
                "Cannot report errors without causing compilation failure"
            liftIO Exit.exitSuccess

runDriverMain :: (es' ~ Driver.DriverEffsRun ++ MainEffs) => Opt -> Eff es' () -> IO ()
runDriverMain opt m = runMain opt $ Driver.runDriver m

printCompileResult :: '[IOE, Reader Opt] :>> es => CallStack -> RunReporter.Reports -> Eff es ()
printCompileResult callStack reports = do
  PrettyUtil.putDoc $
    pIf
      (reports ^. #warnings > 0)
      ( pWarning
          <> ": "
          <> pretty (reports ^. #warnings)
          <> " warnings generated"
      )
      <> Pretty.line
      <> pError
      <> ": Compilation failed due to "
      <> pretty (reports ^. #errors)
      <+> ( if reports ^. #errors == 1
              then "error"
              else "errors"
          )
      <> Pretty.hardline
  whenM (rview @Opt #callStack) $
    PrettyUtil.putDoc $
      Info <#> "Callstack:"
        <> Pretty.line
        <> pretty (prettyCallStack callStack)
