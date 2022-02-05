{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Opt
  ( opt,
  )
where

import Options.Applicative

data Opt = Opt
  { clang :: !Text,
    emitAsm :: !Bool,
    emitLL :: !Bool,
    optimization :: !Int,
    files :: [FilePath]
  }
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
        <> header "This is my header"
    )

parseOpt' :: Parser Opt
parseOpt' = do
  clang <-
    strOption $
      long "clang"
        <> metavar "COMMAND"
        <> help "The path to the clang command to use"
  emitAsm <-
    switch $
      short 'S'
        <> help "Print the assembly"
  emitLL <-
    switch $
      long "emit-llvm"
        <> help "Emit llvm instead of assembly"
  optimization <-
    option auto $
      short 'O'
        <> help "Set the optimization level"
  files <-
    some $
      argument str $
        metavar "FILES"
  pure Opt {clang, emitAsm, emitLL, optimization, files}

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" $ long "version" <> help "Show the version"