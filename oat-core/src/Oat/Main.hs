{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Oat.Main where

import Control.Exception.Safe qualified as Exception
import Data.Text.Encoding.Error (UnicodeException)
import Effectful.Temporary qualified as Temporary
import Oat.Command qualified as Command
import Oat.Common
import Oat.Driver qualified as Driver
import Oat.LL qualified as LL
import Oat.Opt qualified as Opt
import Text.Pretty.Simple (pPrint)
import qualified Data.Text.IO as TIO

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
        runStuff $ do
          contents <- readFileUtf8 file
          liftIO $ TIO.putStrLn contents
          dotSText <- Driver.compileLLText contents
          liftIO $ TIO.putStrLn dotSText
          writeFileUtf8 (opt ^. #output) dotSText
      | otherwise -> do
        Driver.compileLLFile file (opt ^. #output)
          & runStuff

runStuff m =
  m
    & runErrorIO @UnicodeException
    & runErrorIO @[LL.ParseError]
    & Temporary.runTemporary
    & Command.runCommandClangIO
    & runErrorIO @Command.CommandError
