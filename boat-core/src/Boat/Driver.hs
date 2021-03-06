{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Boat.Driver where

import Data.FileEmbed qualified as FileEmbed
import Data.Text qualified as T
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.IO qualified as TIO
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO qualified as FileSystem.IO
import Effectful.Reader.Static
import Effectful.Reader.Static.Optics
import Effectful.Temporary (Temporary)
import Effectful.Temporary qualified as Temporary
import Boat.Backend.X86.Codegen qualified as Backend.X86.Codegen
import Boat.Backend.X86.Pretty qualified as Backend.X86.Pretty
import Boat.Backend.X86.X86 qualified as Backend.X86
import Boat.Cli (Args)
import Boat.Command (Command)
import Boat.Command qualified as Command
import Boat.Error (CompileFail)
import Boat.LL qualified as LL
import Boat.Reporter
import Boat.Reporter qualified as Reporter
import Boat.Utils.Families (type (++))
import Boat.Utils.IO (hPutLnUtf8, hPutUtf8, readFileUtf8, writeFileUtf8)
import Boat.Utils.Monad (whenM)
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as Prettyprinter
import System.FilePath ((-<.>))
import System.FilePath qualified as FilePath
import System.IO qualified
import UnliftIO.Exception qualified as Exception
import UnliftIO.IO qualified as IO
import Acc (Acc)

-- the effects that we have access to in the driver
type DriverEffs :: [Effect]
-- type DriverEffs = IOE ': DriverEffsRun
type DriverEffs =
  '[ IOE,
     Reader Args,
     Reporter [LL.ParseError],
     Reporter [LL.CheckError],
     Error UnicodeException,
     Error CompileFail,
     Temporary,
     FileSystem,
     Command
   ]

drive :: DriverEffs :>> es => Eff es ()
drive = do
  opt <- ask @Args
  file <- case opt ^. #files of
    [p] -> pure p
    _ -> Exception.throwString "Only compiling one file is supported for now"
  if
      | opt ^. #emitAsm -> do
          compileLLFileToAsm file $
            fromMaybe
              (FilePath.takeFileName file -<.> ".s")
              (opt ^. #output)
      | otherwise -> do
          compileLLFile file $
            fromMaybe "a.out" (opt ^. #output)

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

parseLLText ::
  '[ Reader Args,
     Reporter [LL.ParseError],
     Reporter [LL.CheckError],
     Error CompileFail
   ]
    :>> es =>
  Text ->
  Eff es LL.Module
parseLLText text = do
  decls <- LL.parse text LL.mod
  let declMap = LL.declsToMap decls
      mod = LL.Module {decls, declMap}
  whenM (rview @Args #checkLL) $ do
    LL.checkProg mod
  pure mod

parseLLFile :: DriverEffs :>> es => FilePath -> Eff es LL.Module
parseLLFile path = readFileUtf8 path >>= parseLLText

compileLLFile :: DriverEffs :>> es => FilePath -> FilePath -> Eff es ()
compileLLFile llPath out = do
  llText <- readFileUtf8 llPath
  compileLLTextToFile llText out

compileLLTextToFile :: DriverEffs :>> es => Text -> FilePath -> Eff es ()
compileLLTextToFile llText out = do
  asmText <- compileLLText llText
  Temporary.withSystemTempFile "boat.s" $ \asmTemp handle -> do
    FileSystem.IO.hSetBuffering handle IO.NoBuffering
    liftIO $ System.IO.hSetEncoding handle System.IO.utf8
    hPutLnUtf8 handle asmText
    FileSystem.IO.hFlush handle
    compileAsmPaths [asmTemp] out

compileLLText ::
  '[ Reader Args,
     Reporter [LL.ParseError],
     Reporter [LL.CheckError],
     IOE,
     Error CompileFail
   ]
    :>> es =>
  Text ->
  Eff es Text
compileLLText text = do
  insts <- compileLLTextToInsts text
  let mod = Backend.X86.instLabToElems $ toList insts
  let asmDoc = Backend.X86.Pretty.prettyProg mod
  let asmText = Prettyprinter.renderStrict $ Prettyprinter.layoutCompact asmDoc
  pure asmText

compileLLTextToInsts ::
  '[ Reader Args,
     Reporter [LL.ParseError],
     Reporter [LL.CheckError],
     Error CompileFail
   ]
    :>> es =>
  Text ->
  Eff es (Acc Backend.X86.InstLab)
compileLLTextToInsts text = do
  mod <- parseLLText text
  LL.runNameSource $ Backend.X86.Codegen.compileModule mod

-- compile and links all asm paths with the runtime
compileAsmPaths :: DriverEffs :>> es => [FilePath] -> FilePath -> Eff es ()
compileAsmPaths asmFiles out = loop asmFiles []
  where
    loop [] objectTemps = linkRuntime objectTemps out
    loop (asmFile : asmFiles) objectTemps =
      Temporary.withSystemTempFile "boat.o" $ \objectTemp _ -> do
        Command.assemble asmFile objectTemp
        loop asmFiles (objectTemp : objectTemps)

withTextInTemp ::
  '[IOE, Temporary, FileSystem] :>> es =>
  String ->
  Text ->
  (FilePath -> IO.Handle -> Eff es a) ->
  Eff es a
withTextInTemp name text f = Temporary.withSystemTempFile name $ \temp handle -> do
  FileSystem.IO.hSetBuffering handle IO.NoBuffering
  liftIO $ System.IO.hSetEncoding handle System.IO.utf8
  hPutLnUtf8 handle text
  f temp handle

linkRuntime :: DriverEffs :>> es => [FilePath] -> FilePath -> Eff es ()
linkRuntime mods out = do
  Temporary.withSystemTempFile "boat.c" $ \runtimeTemp runtimeHandle -> do
    FileSystem.IO.hSetBuffering runtimeHandle IO.NoBuffering
    liftIO $ System.IO.hSetEncoding runtimeHandle System.IO.utf8
    hPutUtf8 runtimeHandle $ runtimeTemplate "program"
    shouldLinkTestRuntime <- rview @Args #linkTestRuntime
    if shouldLinkTestRuntime
      then withTextInTemp "boat.c" testRuntime $ \testTemp _ -> do
        Command.link (testTemp : runtimeTemp : mods) out
      else Command.link (runtimeTemp : mods) out
  permissions <- FileSystem.getPermissions out
  FileSystem.setPermissions out $ FileSystem.setOwnerExecutable True permissions

compileSingleLLFileClang :: DriverEffs :>> es => FilePath -> FilePath -> Eff es ()
compileSingleLLFileClang llPath out = do
  Temporary.withSystemTempFile "boat.s" $ \asmTemp _ -> do
    Command.compileLlvm llPath asmTemp
    Temporary.withSystemTempFile "boat.o" $ \objectTemp _ -> do
      Command.assemble asmTemp objectTemp
      linkRuntime [objectTemp] out

runtimeTemplate :: Text -> Text
runtimeTemplate programName = T.replace "/* MANGLED_PROGRAM_NAME */" programName template
  where
    template = $(FileEmbed.makeRelativeToProject "../runtime/runtime.c" >>= FileEmbed.embedStringFile)

testRuntime :: Text
testRuntime = $(FileEmbed.makeRelativeToProject "../runtime/test_runtime.c" >>= FileEmbed.embedStringFile)

-- the effects that we need to run
type DriverEffsRun :: [Effect]
type DriverEffsRun =
  '[ Error UnicodeException,
     Temporary,
     FileSystem,
     Command,
     Error Command.CommandError
   ]

data DriverError
  = DriverUnicodeException UnicodeException
  | DriverIOError Exception.IOException
  | DriverCommandError Command.CommandError
  deriving (Show, Eq)

runDriver ::
  ( es' ~ DriverEffsRun ++ es,
    '[ IOE,
       Reporter [DriverError],
       Error CompileFail,
       Reader Args
     ]
      :>> es
  ) =>
  Eff es' a ->
  Eff es a
runDriver m = do
  let run =
        Error.runErrorNoCallStack @UnicodeException
          >>> Temporary.runTemporary
          >>> FileSystem.runFileSystem
          >>> Command.runCommandClangIO
          >>> Error.runErrorNoCallStack @Command.CommandError
  res <- run m
  case res of
    Left e -> Reporter.reportFail [DriverCommandError e]
    Right res' -> case res' of
      Left ex -> Reporter.reportFail [DriverUnicodeException ex]
      Right res'' -> pure res''
