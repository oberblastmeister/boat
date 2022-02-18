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

import UnliftIO.Exception qualified as Exception
import Effectful.Error.Static
import Effectful.Process qualified as Process

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
runCommandClangIO = interpret $ \_ -> \case
  Assemble {dotS, dotO} -> proc "clang" ["-c", dotS, "-o", dotO]
  Preprocess {dotOat, dotI} -> proc "cpp" ["-E", dotOat, dotI]
  CompileLlvm {dotLL, dotS} -> proc "clang" ["-S", dotLL, "-o", dotS]
  Link {mods, out} -> proc "clang" $ mods ++ ["-o", out]
  where
    proc cmd args = adapt $ Process.callProcess cmd args
    adapt m = Process.runProcess m `Exception.catch` (throwError . CommandError)
