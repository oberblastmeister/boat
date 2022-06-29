module Boat.Command
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
import System.Process.Typed (proc, runProcess_)
import UnliftIO.Exception qualified as Exception

data Command :: Effect where
  Assemble :: {dotS :: FilePath, dotO :: FilePath} -> Command m ()
  Preprocess :: {dotBoat :: FilePath, dotI :: FilePath} -> Command m ()
  CompileLlvm :: {dotLL :: FilePath, dotS :: FilePath} -> Command m ()
  Link :: {mods :: [FilePath], out :: FilePath} -> Command m ()

type instance DispatchOf Command = 'Dynamic

assemble :: Command :> es => FilePath -> FilePath -> Eff es ()
assemble dotS dotO = send Assemble {dotS, dotO}

preprocess :: Command :> es => FilePath -> FilePath -> Eff es ()
preprocess dotBoat dotI = send Preprocess {dotBoat, dotI}

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
  Preprocess {dotBoat, dotI} -> run $ proc "cpp" ["-E", dotBoat, dotI]
  CompileLlvm {dotLL, dotS} -> clang ["-S", dotLL, "-o", dotS]
  Link {mods, out} -> clang $ mods ++ ["-o", out]
  where
    clang opts' = run $ proc "clang" (opts' ++ opts)
    run = adapt . runProcess_ @(Eff es)
    adapt m = m `Exception.catch` (Error.throwError . CommandError)
