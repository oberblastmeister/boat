{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Oat.Main where

import Control.Exception.Safe qualified as Exception
import Effectful.Reader.Static
import Oat.Driver qualified as Driver
import Oat.Opt (Opt)
import Oat.Opt qualified as Opt
import Oat.PrettyUtil (Ann)
import Oat.PrettyUtil qualified as PrettyUtil
import Prettyprinter (Doc)
import System.Exit qualified as Exit
import System.FilePath ((-<.>))
import System.FilePath qualified as FilePath

main :: IO ()
main = runEff run

run :: IOE :> es => Eff es ()
run = Opt.opt >>= runWithOpt

runWithOpt :: IOE :> es => Opt -> Eff es ()
runWithOpt opt = do
  runReader opt tryRun >>= \case
    Left doc -> do
      PrettyUtil.putDoc doc
      liftIO Exit.exitFailure
    Right () -> do
      liftIO Exit.exitSuccess

tryRun :: '[IOE, Reader Opt] :>> es => Eff es (Either (Doc Ann) ())
tryRun = do
  opt <- ask @Opt
  file <- case opt ^. #files of
    [p] -> pure p
    _ -> Exception.throwString "Only compiling one file is supported for now"
  if
      | opt ^. #emitAsm ->
        Driver.runEffs $
          Driver.compileLLFileToAsm file $
            fromMaybe (FilePath.takeFileName file -<.> ".s") (opt ^. #output)
      | otherwise ->
        Driver.runEffs $
          Driver.compileLLFile file $
            fromMaybe "a.out" (opt ^. #output)
