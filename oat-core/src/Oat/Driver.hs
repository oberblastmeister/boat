{-# LANGUAGE QualifiedDo #-}

module Oat.Driver where

import Control.OnLeft (OnLeft (OnLeft))
import Control.OnLeft qualified as OnLeft
import Data.Text.Encoding.Error (UnicodeException)
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.Temporary (Temporary)
import Effectful.Temporary qualified as Temporary
import Oat.Backend.X86.Codegen qualified as Backend.X86.Codegen
import Oat.Backend.X86.Pretty qualified as Backend.X86.Pretty
import Oat.Backend.X86.X86 qualified as Backend.X86
import Oat.Command (Command)
import Oat.Command qualified as Command
import Oat.Common (liftEither, readFileUtf8, runErrorIO, writeFileUtf8, type (++))
import Oat.LL qualified as LL
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as Prettyprinter

-- the effects that we have access to in the driver
type DriverEffs :: [Effect]
type DriverEffs = IOE ': DriverEffsRun

compileLLFileToAsm :: DriverEffs :>> es => FilePath -> FilePath -> Eff es ()
compileLLFileToAsm dotLL dotS = do
  contents <- readFileUtf8 dotLL
  dotSText <- compileLLText contents
  writeFileUtf8 dotS dotSText

parseLL :: (Error [LL.ParseError] :> es) => Text -> Eff es LL.Prog
parseLL text = liftEither $ LL.parse text LL.prog

parseLLFile :: DriverEffs :>> es => FilePath -> Eff es LL.Prog
parseLLFile path = readFileUtf8 path >>= parseLL

compileLLFile :: DriverEffs :>> es => FilePath -> FilePath -> Eff es ()
compileLLFile dotLL dotO = do
  llText <- readFileUtf8 dotLL
  compileLLTextToFile llText dotO

compileLLTextToFile :: DriverEffs :>> es => Text -> FilePath -> Eff es ()
compileLLTextToFile llText dotO = do
  dotSText <- compileLLText llText
  Temporary.withSystemTempFile "oat" $ \dotSTemp _ -> do
    writeFileUtf8 dotSTemp dotSText
    compileAsm True dotSTemp dotO

compileLLText :: '[Error [LL.ParseError]] :>> es => Text -> Eff es Text
compileLLText text = do
  insts <- compileLLTextToInsts text
  let !_ = traceShowId insts
  let prog = Backend.X86.instLabToElems $ toList insts
  let !_ = traceShowId prog
  let asmDoc = Backend.X86.Pretty.prettyProg prog
  let asmText = Prettyprinter.renderStrict $ Prettyprinter.layoutCompact asmDoc
  pure asmText

compileLLTextToInsts :: '[Error [LL.ParseError]] :>> es => Text -> Eff es (Seq Backend.X86.InstLab)
compileLLTextToInsts text = do
  insts <- parseLL text
  let !_ = traceShowId insts
  LL.runNameSource $ Backend.X86.Codegen.compileProg insts

compileAsm :: '[Command, Temporary] :>> es => Bool -> FilePath -> FilePath -> Eff es ()
compileAsm shouldLink dotS dotO = do
  Temporary.withSystemTempFile "oat" $ \dotOTemp _ -> do
    Command.assemble dotS dotOTemp
    when shouldLink $
      Command.link [dotOTemp] dotO

compileLLFileClang :: '[Command, Temporary] :>> es => FilePath -> FilePath -> Eff es ()
compileLLFileClang dotLL dotO = do
  Temporary.withSystemTempFile "oat" $ \dotSTemp _ -> do
    Command.compileLlvm dotLL dotSTemp
    compileAsm True dotSTemp dotO

-- the effects that we need to run
type DriverEffsRun :: [Effect]
type DriverEffsRun =
  '[ Error [LL.ParseError],
     Error UnicodeException,
     Temporary,
     FileSystem,
     Command,
     Error Command.CommandError
   ]

runEffs :: IOE :> es => Eff (DriverEffsRun ++ es) a -> Eff es (Either String a)
runEffs m = do
  res <- run m
  pure $ OnLeft.do
    res <- OnLeft show res
    res <- OnLeft show res
    res <- OnLeft show res
    return res
  where
    run =
      runErrorNoCallStack @[LL.ParseError]
        >>> runErrorNoCallStack @UnicodeException
        >>> Temporary.runTemporary
        >>> FileSystem.runFileSystem
        >>> Command.runCommandClangIO
        >>> runErrorNoCallStack @Command.CommandError
