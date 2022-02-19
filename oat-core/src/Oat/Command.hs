module Oat.Command
  ( Command (..),
    CommandError (..),
    assemble,
    preprocess,
    compileLlvm,
    link,
    runCommandClangIO,
  )
where

import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Process qualified as Process
import UnliftIO.Exception qualified as Exception

data Command :: Effect where
  Assemble :: {dotS :: FilePath, dotO :: FilePath} -> Command m ()
  Preprocess :: {dotOat :: FilePath, dotI :: FilePath} -> Command m ()
  CompileLlvm :: {dotLL :: FilePath, dotS :: FilePath} -> Command m ()
  Link :: {mods :: [FilePath], out :: FilePath} -> Command m ()

type instance DispatchOf Command = 'Dynamic

assemble :: Command :> es => FilePath -> FilePath -> Eff es ()
assemble dotS dotO = send Assemble {dotS, dotO}

preprocess :: Command :> es => FilePath -> FilePath -> Eff es ()
preprocess dotOat dotI = send Preprocess {dotOat, dotI}

compileLlvm :: Command :> es => FilePath -> FilePath -> Eff es ()
compileLlvm dotLL dotS = send CompileLlvm {dotLL, dotS}

link :: Command :> es => [FilePath] -> FilePath -> Eff es ()
link mods out = send Link {mods, out}

data CommandError = CommandError Exception.IOException
  deriving (Show, Eq)

runCommandClangIO :: ('[IOE, Error CommandError] :>> es) => Eff (Command ': es) a -> Eff es a
runCommandClangIO =
  runCommandClangIOWith
    [ -- errors like this happen in nix shell
      "-Wno-unused-command-line-argument"
    ]

-- run with extra arguments
runCommandClangIOWith :: forall es a. ('[IOE, Error CommandError] :>> es) => [String] -> Eff (Command ': es) a -> Eff es a
runCommandClangIOWith opts = interpret $ \_ -> \case
  Assemble {dotS, dotO} -> clang ["-c", dotS, "-o", dotO]
  Preprocess {dotOat, dotI} -> proc "cpp" ["-E", dotOat, dotI]
  CompileLlvm {dotLL, dotS} -> clang ["-S", dotLL, "-o", dotS]
  Link {mods, out} -> clang $ mods ++ ["-o", out]
  where
    clang opts' = proc "clang" (opts' ++ opts)
    proc :: FilePath -> [String] -> Eff es ()
    proc cmd args = adapt $ Process.callProcess cmd args
    adapt m = Process.runProcess m `Exception.catch` (Error.throwError . CommandError)
