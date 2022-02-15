{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Opt
  ( Opt (..),
    opt,
    defOpt,
  )
where

import Options.Applicative

data Opt = Opt
  { clang :: !Text,
    emitAsm :: !Bool,
    emitLL :: !Bool,
    checkLL :: !Bool,
    optimization :: !Optimization,
    files :: [FilePath],
    regAllocKind :: !RegAllocKind,
    output :: Maybe FilePath,
    callStack :: !Bool
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

$(makeFieldLabelsNoPrefix ''Opt)

opt :: IOE :> es => Eff es Opt
opt = liftIO $ customExecParser optPrefs parseOpt

optPrefs :: ParserPrefs
optPrefs =
  prefs $
    disambiguate
      <> showHelpOnError
      <> showHelpOnEmpty

parseOpt :: ParserInfo Opt
parseOpt =
  info
    (helper <*> versionOption <*> parseOpt')
    ( fullDesc
        <> progDesc "Compiler for the oat language"
    )

parseOpt' :: Parser Opt
parseOpt' = do
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
  pure Opt {clang, emitAsm, emitLL, checkLL, optimization, files, regAllocKind, output, callStack}

defOpt :: Opt
defOpt =
  Opt
    { clang = "clang",
      emitAsm = False,
      emitLL = False,
      checkLL = False,
      optimization = O1,
      files = [],
      regAllocKind = GraphReg,
      output = Nothing,
      callStack = False
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
