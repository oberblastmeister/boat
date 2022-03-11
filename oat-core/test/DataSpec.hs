{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DataSpec where

import Conduit (runConduit, runConduitRes, (.|))
import Control.Arrow ((^>>))
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LByteString
import Data.Conduit.Combinators qualified as C
import Data.HashSet qualified as HashSet
import Data.IORef qualified as IORef
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Lazy qualified as LText
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.Process qualified as Process
import Effectful.State.Static.Local (evalState)
import Effectful.Temporary (Temporary)
import Effectful.Temporary qualified as Temporary
import Oat.Cli qualified as Cli
import Oat.Dataflow qualified as Dataflow
import Oat.Dataflow.Label qualified as Dataflow.Label
import Oat.Driver qualified as Driver
import Oat.Error (CompileFail)
import Oat.LL qualified as LL
import Oat.LL.AstToIr qualified as LL.AstToIr
import Oat.LL.Lexer qualified as LL.Lexer
import Oat.LL.Pass.Live qualified as LL.Pass.Live
import Oat.Main qualified as Main
import Oat.Reporter qualified as Reporter
import Oat.Utils.IO (hPutLnUtf8, listDirectory', readFileUtf8, runErrorIO, writeFileLnUtf8)
import Oat.Utils.Impossible (impossible)
import Oat.Utils.Misc (timSort)
import Oat.Utils.Optics (unwrap)
import System.FilePath ((-<.>), (</>))
import System.FilePath qualified as FilePath
import System.Process.Typed (proc, readProcessStdout)
import Test.Hspec
import Test.Hspec.Core.Runner qualified as Spec
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Core.Spec qualified as Spec
import Text.Pretty.Simple (pShowNoColor)
import UnliftIO.Directory qualified as Directory
import UnliftIO.Exception qualified as Exception
import UnliftIO.IO qualified as IO
import qualified Oat.Dataflow.Fuel as Dataflow.Fuel

data Config = Config
  { update :: !Bool,
    parallel :: !Bool,
    firstTimeUpdate :: !Bool,
    filter :: Pred
  }

type Pred = FilePath -> Bool

(<&&>) :: Pred -> Pred -> Pred
pred <&&> pred' = (&&) <$> pred <*> pred'

(<||>) :: Pred -> Pred -> Pred
pred <||> pred' = (||) <$> pred <*> pred'

(<//>) :: FilePath -> Pred -> Pred
path <//> pred = FilePath.makeRelative path ^>> pred

infixr 6 <&&>, <||>

infixr 5 <//>

predNot :: Pred -> Pred
predNot = fmap not

filterPred :: Pred -> [FilePath] -> [FilePath]
filterPred = filter

$(makeFieldLabelsNoPrefix ''Config)

spec :: Spec
spec = do
  specWith
    defConfig

run :: Config -> IO ()
run config = Spec.hspecWith Spec.defaultConfig $ specWith config

specWith :: Config -> Spec
specWith config = do
  llLexerSpec config
  llParserSpec config
  llCompileSpec config
  llLiveSpec config

llLexerSpec :: Config -> Spec
llLexerSpec config = do
  makeSnapshotSpec
    config {parallel = True}
    ( pure
        . LText.toStrict
        . pShowNoColor
        . LL.Lexer.tokenize
    )
    "ll_lexer/ok"

llParserSpec :: Config -> Spec
llParserSpec config = do
  makeSnapshotSpec
    config {parallel = True}
    ( \text -> do
        let (res, parseErrors) =
              runPureEff $
                Reporter.runReporterList @LL.ParseError $
                  Error.runErrorNoCallStack @CompileFail $
                    LL.parse text LL.mod
        case res of
          Left _ -> Exception.throwString $ LText.unpack $ pShowNoColor parseErrors
          Right res -> pure $ LText.toStrict $ pShowNoColor res
    )
    "ll_parser/ok"

llCompileSpec :: Config -> Spec
llCompileSpec config = do
  makeSnapshotSpec
    config {parallel = False}
    ( \text -> Temporary.runTemporary $ do
        Temporary.withSystemTempFile "oat" $ \temp handle -> do
          liftIO $
            Main.runDriverMain_
              Cli.defArgs
                { Cli.linkTestRuntime = True,
                  Cli.checkLL = True
                }
              $ Driver.compileLLTextToFile text temp
          IO.hClose handle
          (exit, stdout) <- readProcessStdout (proc temp [])
          pure $ Text.Encoding.decodeUtf8 $ LByteString.toStrict stdout
    )
    "ll_compile/ok"

llLiveSpec :: Config -> Spec
llLiveSpec config = do
  makeSnapshotSpec
    config {parallel = False}
    ( \text -> do
        (decls, errors) <-
          runErrorIO @CompileFail $
            Reporter.runReporterList
              @LL.ParseError
              $ LL.parse text LL.mod
        case errors of
          _ : _ -> Exception.throwString $ LText.unpack $ pShowNoColor errors
          _ -> pure ()
        let declMap = LL.declsToMap decls
            mod = LL.Module {decls, declMap}
            fun :: LL.FunDecl = declMap ^. #funDecls % #map % at "test" % unwrap impossible
            irFun =
              runPureEff $
                Dataflow.Label.runLabelSource $
                  evalState @(Dataflow.LabelMap LL.Name)
                    mempty
                    $ evalState
                      @(HashMap LL.Name Dataflow.Label)
                      mempty
                      $ LL.AstToIr.funBodyToIr fun.body
            !_ = dbg irFun
            res = runPureEff $ Dataflow.Fuel.runFuelInfinite $ LL.Pass.Live.run irFun
        pure $ LText.toStrict $ pShowNoColor res
    )
    "ll_live"

-- | cleans .expect files that do not have a matching file
clean :: IO ()
clean = do
  runConduitRes $
    C.sourceDirectoryDeep False "test_data"
      .| C.iterM
        ( \path ->
            when (FilePath.takeExtension path == ".s") $
              Directory.removeFile path
        )
      .| C.filter (\path -> FilePath.takeExtension path == ".expect")
      .| C.mapM_
        ( \expectPath -> do
            anyExist <-
              runConduit $
                C.yieldMany ((expectPath -<.>) <$> possibleExtensions)
                  .| C.mapM Directory.doesFileExist
                  .| C.any id
            unless anyExist $ do
              Directory.removeFile expectPath
        )

runLL :: FilePath -> IO ()
runLL path = do
  emitAsm path
  runAsm $ FilePath.takeFileName path -<.> ".s"

-- | the possible extensions for each .expect file
possibleExtensions :: [String]
possibleExtensions = [".ll", ".oat"]

emitAsm :: FilePath -> IO ()
emitAsm path =
  Main.mainWithArgs_ $
    Cli.defArgs
      { Cli.emitAsm = True,
        Cli.checkLL = True,
        Cli.files = ["test_data/ll_compile" </> path],
        Cli.output = Just $ "test_data/asm_compile" </> FilePath.takeFileName path -<.> ".s"
      }

showAsm :: FilePath -> IO ()
showAsm path = do
  emitAsm path
  runConduitRes $ C.sourceFileBS ("test_data/asm_compile" </> FilePath.takeFileName path) .| C.stdout
  ByteString.Char8.putStrLn ""

openAsm :: FilePath -> IO ()
openAsm path = do
  emitAsm path
  runEff $
    Process.runProcess $ do
      Process.callProcess "code" ["test_data/asm_compile" </> FilePath.takeFileName path -<.> ".s"]

compileAsm :: FilePath -> IO ()
compileAsm path = do
  asmDir <- Directory.makeAbsolute "test_data/asm_compile"
  let exePath = asmDir </> FilePath.takeBaseName path
  Main.runDriverMain_ Cli.defArgs {Cli.linkTestRuntime = True} $
    Driver.compileAsmPaths [asmDir </> path] exePath

runAsm :: FilePath -> IO ()
runAsm path = do
  compileAsm path
  asmDir <- Directory.makeAbsolute "test_data/asm_compile"
  runEff $ Process.runProcess $ Process.callProcess (asmDir </> FilePath.takeBaseName path) []

openLLData :: FilePath -> IO ()
openLLData path = runEff $ Process.runProcess $ Process.callProcess "code" ["test_data/ll_compile" </> path]

matchHead :: FilePath -> Pred
matchHead base path = FilePath.takeDirectory path == base

matchTail :: FilePath -> Pred
matchTail tail path = FilePath.takeFileName path == tail

matchExact :: [FilePath] -> Pred
matchExact ps = \p -> has (ix p) psSet
  where
    psSet = HashSet.fromList ps

defConfig :: Config
defConfig =
  Config
    { update = False,
      firstTimeUpdate = True,
      -- filter = matchTail "first.ll" <&&> predNot (matchHead "ll_compile/ok"),
      -- filter = matchTail "retry.ll" <&&> predNot (matchHead "ll_compile/ok"),
      filter = predNot (matchTail "gep1.ll"),
      parallel = False
    }

makeSnapshotSpec :: Config -> (Text -> Eff '[IOE] Text) -> FilePath -> Spec
makeSnapshotSpec config convert dirPath = do
  llCompileOk <-
    runIOE
      . FileSystem.runFileSystem
      . listDirectory'
      $ "test_data" </> dirPath
  let pred = "test_data/" <//> (notExpectPath <&&> config ^. #filter)
  (if config ^. #parallel then parallel else id) $
    describe dirPath $
      traverse_
        (makeSnapshotFileSpec config convert)
        (timSort $ filterPred pred llCompileOk)

notExpectPath :: Pred
notExpectPath path = FilePath.takeExtension path /= ".expect"

makeSnapshotFileSpec :: Config -> (Text -> Eff '[IOE] Text) -> FilePath -> Spec
makeSnapshotFileSpec config convert path = it (FilePath.takeFileName path) $ IOResultSpec $ run example
  where
    run =
      runEff
        . FileSystem.runFileSystem
        . Temporary.runTemporary
        . runErrorIO @UnicodeException

    example :: '[IOE, FileSystem, Temporary, Error UnicodeException] :>> es => Eff es Spec.Result
    example = do
      contents <- readFileUtf8 path
      let expectPath = path -<.> "expect"
      existsExpect <- FileSystem.doesFileExist expectPath
      convertContents <- liftIO $ runEff $ convert contents
      if existsExpect
        then do
          expectContents <- readFileUtf8 expectPath <&> stripLastNewline
          if convertContents /= expectContents
            then
              if config ^. #update
                then do
                  writeFileLnUtf8 expectPath convertContents
                  pure
                    Spec.Result
                      { resultInfo = "Updating the .expect file!",
                        resultStatus = Spec.Success
                      }
                else do
                  Temporary.withSystemTempFile "snap" $ \temp handle -> do
                    hPutLnUtf8 handle convertContents
                    IO.hFlush handle
                    gitDiff expectPath temp
                  pure
                    Spec.Result
                      { resultInfo = "The files did not match!",
                        resultStatus = Spec.Failure Nothing Spec.NoReason
                      }
            else
              pure
                Spec.Result
                  { resultInfo = "The files matched!",
                    resultStatus = Spec.Success
                  }
        else
          if config ^. #firstTimeUpdate
            then do
              writeFileLnUtf8 expectPath convertContents
              pure
                Spec.Result
                  { resultInfo = "First time running. Updating the .expect file!",
                    resultStatus = Spec.Success
                  }
            else
              pure
                Spec.Result
                  { resultInfo = "There was no .expect file!",
                    resultStatus = Spec.Failure Nothing Spec.NoReason
                  }

gitDiff :: MonadIO m => FilePath -> FilePath -> m ()
gitDiff file file' = do
  liftIO $
    runEff $
      Process.runProcess $
        Process.callProcess
          "git"
          [ "--no-pager",
            "diff",
            "--no-index",
            "--color=always",
            file,
            file'
          ]

stripLastNewline :: Text -> Text
stripLastNewline (t :> '\n') = t
stripLastNewline t = t

runIOE :: Eff '[IOE] a -> SpecM arg a
runIOE = runIO . runEff

newtype IOResultSpecWith arg = IOResultSpecWith (arg -> IO Spec.Result)

newtype IOResultSpec = IOResultSpec (IO Spec.Result)

instance Spec.Example (IOResultSpecWith arg) where
  type Arg (IOResultSpecWith arg) = arg

  evaluateExample (IOResultSpecWith example) _params action _progress = do
    ref <- IORef.newIORef (Spec.Result "" Spec.Success)
    action $ \arg -> do
      res <- example arg
      IORef.writeIORef ref res
    IORef.readIORef ref

instance Spec.Example IOResultSpec where
  type Arg IOResultSpec = ()

  evaluateExample (IOResultSpec e) = Spec.evaluateExample (IOResultSpecWith $ const e)
