{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Oat.Driver where

import Control.Exception.Safe qualified as Exception
import Control.OnLeft (OnLeft (OnLeft))
import Control.OnLeft qualified as OnLeft
import Data.FileEmbed qualified as FileEmbed
import Data.Text qualified as T
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.IO qualified as TIO
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO qualified as FileSystem.IO
import Effectful.Reader.Static
import Effectful.Reader.Static.Optics
import Effectful.Temporary (Temporary)
import Effectful.Temporary qualified as Temporary
import Oat.Backend.X86.Codegen qualified as Backend.X86.Codegen
import Oat.Backend.X86.Pretty qualified as Backend.X86.Pretty
import Oat.Backend.X86.X86 qualified as Backend.X86
import Oat.Command (Command)
import Oat.Command qualified as Command
import Oat.Common (hPutUtf8, liftEither, maybeToLeft, readFileUtf8, whenM, writeFileUtf8, type (++))
import Oat.LL qualified as LL
import Oat.Opt (Opt)
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as Prettyprinter
import System.IO qualified as IO

-- the effects that we have access to in the driver
type DriverEffs :: [Effect]
-- type DriverEffs = IOE ': DriverEffsRun
type DriverEffs =
  '[ IOE,
     Reader Opt,
     Error [LL.ParseError],
     Error (NonEmpty LL.CheckError),
     Error UnicodeException,
     Temporary,
     FileSystem,
     Command
   ]

compileLLFileToAsm :: DriverEffs :>> es => FilePath -> FilePath -> Eff es ()
compileLLFileToAsm llPath asmPath = do
  llText <- readFileUtf8 llPath
  asmText <- compileLLText llText
  writeFileUtf8 asmPath asmText

compileLLFileToAsmShow :: DriverEffs :>> es => FilePath -> Eff es ()
compileLLFileToAsmShow llPath = do
  contents <- readFileUtf8 llPath
  asmText <- compileLLText contents
  liftIO $ TIO.putStrLn asmText

parseLLText :: '[Reader Opt, Error [LL.ParseError], Error (NonEmpty LL.CheckError)] :>> es => Text -> Eff es LL.Prog
parseLLText text = do
  decls <- liftEither $ LL.parse text LL.prog
  let declMap = LL.declsToMap decls
      prog = LL.Prog {decls, declMap}
  whenM (rview @Opt #checkLL) $ do
    liftEither $ maybeToLeft () $ LL.checkProg prog
  pure prog

parseLLFile :: DriverEffs :>> es => FilePath -> Eff es LL.Prog
parseLLFile path = readFileUtf8 path >>= parseLLText

compileLLFile :: DriverEffs :>> es => FilePath -> FilePath -> Eff es ()
compileLLFile llPath out = do
  llText <- readFileUtf8 llPath
  compileLLTextToFile llText out

compileLLTextToFile :: DriverEffs :>> es => Text -> FilePath -> Eff es ()
compileLLTextToFile llText out = do
  asmText <- compileLLText llText
  Temporary.withSystemTempFile "oat.s" $ \asmTemp handle -> do
    FileSystem.IO.hSetBuffering handle IO.NoBuffering
    liftIO $ IO.hSetEncoding handle IO.utf8
    hPutUtf8 handle asmText
    FileSystem.IO.hFlush handle
    compileAsmPaths [asmTemp] out

compileLLText :: '[Reader Opt, Error [LL.ParseError], Error (NonEmpty LL.CheckError)] :>> es => Text -> Eff es Text
compileLLText text = do
  insts <- compileLLTextToInsts text
  let prog = Backend.X86.instLabToElems $ toList insts
  let asmDoc = Backend.X86.Pretty.prettyProg prog
  let asmText = Prettyprinter.renderStrict $ Prettyprinter.layoutCompact asmDoc
  pure asmText

compileLLTextToInsts :: '[Reader Opt, Error [LL.ParseError], Error (NonEmpty LL.CheckError)] :>> es => Text -> Eff es (Seq Backend.X86.InstLab)
compileLLTextToInsts text = do
  prog <- parseLLText text
  LL.runNameSource $ Backend.X86.Codegen.compileProg prog

-- compile and links all asm paths with the runtime
compileAsmPaths :: DriverEffs :>> es => [FilePath] -> FilePath -> Eff es ()
compileAsmPaths asmFiles out = loop asmFiles []
  where
    loop [] objectTemps = linkRuntime objectTemps out
    loop (asmFile : asmFiles) objectTemps =
      Temporary.withSystemTempFile "oat.o" $ \objectTemp _ -> do
        Command.assemble asmFile objectTemp
        loop asmFiles (objectTemp : objectTemps)

linkRuntime :: DriverEffs :>> es => [FilePath] -> FilePath -> Eff es ()
linkRuntime mods out = do
  Temporary.withSystemTempFile "oat.c" $ \runtimeTemp runtimeHandle -> do
    FileSystem.IO.hSetBuffering runtimeHandle IO.NoBuffering
    liftIO $ IO.hSetEncoding runtimeHandle IO.utf8
    hPutUtf8 runtimeHandle $ runtimeTemplate "program"
    Command.link (runtimeTemp : mods) out
  permissions <- FileSystem.getPermissions out
  FileSystem.setPermissions out $ FileSystem.setOwnerExecutable True permissions

compileSingleLLFileClang :: DriverEffs :>> es => FilePath -> FilePath -> Eff es ()
compileSingleLLFileClang llPath out = do
  Temporary.withSystemTempFile "oat.s" $ \asmTemp _ -> do
    Command.compileLlvm llPath asmTemp
    Temporary.withSystemTempFile "oat.o" $ \objectTemp _ -> do
      Command.assemble asmTemp objectTemp
      linkRuntime [objectTemp] out

runtimeTemplate :: Text -> Text
runtimeTemplate programName =
  T.replace "/* MANGLED_PROGRAM_NAME */" programName template
  where
    template = $(FileEmbed.makeRelativeToProject "../runtime/runtime.c" >>= FileEmbed.embedStringFile)

-- the effects that we need to run
type DriverEffsRun :: [Effect]
type DriverEffsRun =
  '[ Error [LL.ParseError],
     Error (NonEmpty LL.CheckError),
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
    res <- OnLeft show res
    return res
  where
    run =
      runErrorNoCallStack @[LL.ParseError]
        >>> runErrorNoCallStack @(NonEmpty LL.CheckError)
        >>> runErrorNoCallStack @UnicodeException
        >>> Temporary.runTemporary
        >>> FileSystem.runFileSystem
        >>> Command.runCommandClangIO
        >>> runErrorNoCallStack @Command.CommandError

runEffs' :: IOE :> es => Eff (DriverEffsRun ++ es) a -> Eff es a
runEffs' m =
  runEffs m >>= \case
    Left e -> Exception.throwString e
    Right a -> pure a
