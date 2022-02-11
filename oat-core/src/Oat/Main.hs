{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Oat.Main where

import Control.Exception.Safe qualified as Exception
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.IO qualified as TIO
import Effectful.FileSystem qualified as FileSystem
import Effectful.Temporary qualified as Temporary
import Oat.Command qualified as Command
import Oat.Common
import Oat.Driver qualified as Driver
import Oat.LL qualified as LL
import Oat.Opt qualified as Opt
import System.FilePath ((-<.>))

main :: IO ()
main = runEff run

run :: IOE :> es => Eff es ()
run = do
  opt <- Opt.opt
  file <- case opt ^. #files of
    [p] -> pure p
    _ -> Exception.throwString "Only compiling one file is supported for now"
  if
      | opt ^. #emitAsm -> do
        Driver.runEffs $ do
          contents <- readFileUtf8 file
          liftIO $ TIO.putStrLn contents
          dotSText <- Driver.compileLLText contents
          liftIO $ TIO.putStrLn dotSText
          let outputPath = fromMaybe (file -<.> ".s") (opt ^. #output)
          writeFileUtf8 outputPath dotSText
      | otherwise -> do
        let outputPath = fromMaybe "a.out" (opt ^. #output)
        Driver.runEffs $ Driver.compileLLFile file outputPath
