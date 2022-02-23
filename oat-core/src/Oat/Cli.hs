{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Cli
  ( Args (..),
    args,
    defArgs,
  )
where

import Options.Applicative

data Args = Args
  { clang :: !Text,
    emitAsm :: !Bool,
    emitLL :: !Bool,
    checkLL :: !Bool,
    optimization :: !Optimization,
    files :: [FilePath],
    regAllocKind :: !RegAllocKind,
    output :: Maybe FilePath,
    callStack :: !Bool,
    linkTestRuntime :: !Bool
  }
  deriving (Show, Eq)

data RegAllocKind
  = NoReg
  | SimpleReg
  | GraphReg
  | LinearReg
  deriving (Show, Eq)

data Optimization
  = O1
  | O2
  | O3
  deriving (Show, Eq)

$(makeFieldLabelsNoPrefix ''Args)

args :: IO Args
args = customExecParser optPrefs parseArgs

optPrefs :: ParserPrefs
optPrefs =
  prefs $
    disambiguate
      <> showHelpOnError
      <> showHelpOnEmpty

parseArgs :: ParserInfo Args
parseArgs =
  info
    (helper <*> versionOption <*> parseArgs')
    ( fullDesc
        <> progDesc "Compiler for the oat language"
    )

parseArgs' :: Parser Args
parseArgs' = do
  clang <-
    strOption $
      long "clang"
        <> metavar "COMMAND"
        <> value "clang"
        <> help "The path to the clang command to use"
  emitAsm <-
    switch $
      short 'S'
        <> help "Print the assembly"
  emitLL <-
    switch $
      long "emit-ll"
        <> help "Emit llvm instead of assembly"
  checkLL <-
    switch $
      long "check-ll"
        <> help "Check llvm the"
  optimization <-
    option parseOptimization $
      short 'O'
        <> value O1
        <> help "Set the optimization level"
  files <-
    some $
      argument str $
        metavar "FILES"
  regAllocKind <-
    option parseRegAllocKind $
      long "reg-alloc"
        <> value GraphReg
  output <-
    option (Just <$> auto) $
      short 'o'
        <> value Nothing
        <> help "Set the output file"
  callStack <-
    switch $
      long "callstack"
        <> help "Show the callstack on errors"
  linkTestRuntime <-
    switch $
      long "link-test-runtime"
  pure
    Args
      { clang,
        emitAsm,
        emitLL,
        checkLL,
        optimization,
        files,
        regAllocKind,
        output,
        callStack,
        linkTestRuntime
      }

defArgs :: Args
defArgs =
  Args
    { clang = "clang",
      emitAsm = False,
      emitLL = False,
      checkLL = False,
      optimization = O1,
      files = [],
      regAllocKind = GraphReg,
      output = Nothing,
      callStack = False,
      linkTestRuntime = False
    }

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" $ long "version" <> help "Show the version"

parseRegAllocKind :: ReadM RegAllocKind
parseRegAllocKind = eitherReader $ \case
  "no" -> Right NoReg
  "simple" -> Right SimpleReg
  "graph" -> Right GraphReg
  "linear" -> Right LinearReg
  _ -> Left "Invalid register allocator method"

parseOptimization :: ReadM Optimization
parseOptimization = eitherReader $ \case
  "1" -> Right O1
  "2" -> Right O2
  "3" -> Right O3
  _ -> Left "Invalid optimization level"
