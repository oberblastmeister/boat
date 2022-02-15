{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Oat.Main where

import Control.Exception.Safe qualified as Exception
import Effectful.Reader.Static
import Oat.Driver qualified as Driver
import Oat.Opt (Opt)
import Oat.Opt qualified as Opt
import System.FilePath ((-<.>))
import System.FilePath qualified as FilePath

main :: IO ()
main = runEff run

run :: IOE :> es => Eff es ()
run = do
  opt <- Opt.opt
  runReader opt tryRun >>= \case
    Left e -> liftIO $ putStrLn e
    Right () -> pure ()

tryRun :: '[IOE, Reader Opt] :>> es => Eff es (Either String ())
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
