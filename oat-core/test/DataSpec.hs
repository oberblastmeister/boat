{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DataSpec where

import Conduit (runConduit, runConduitRes, (.|))
import Control.Exception.Safe qualified as Exception
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Conduit.Combinators qualified as C
import Data.Foldable (traverse_)
import Data.HashSet qualified as HashSet
import Data.IORef qualified as IORef
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Lazy qualified as LText
import Effectful.FileSystem qualified as FileSystem
import Effectful.Process qualified as Process
import Effectful.Reader.Static
import Effectful.Temporary qualified as Temporary
import Oat.Common (hPutUtf8, runErrorIO, writeFileUtf8)
import Oat.Common qualified
import Oat.Driver qualified as Driver
import Oat.LL.Lexer qualified as LL.Lexer
import Oat.LL.Parser qualified as LL.Parser
import Oat.LL.ParserWrapper qualified as LL.ParserWrapper
import Oat.Opt qualified as Opt
import System.Directory qualified as Directory
import System.FilePath ((-<.>), (</>))
import System.FilePath qualified as FilePath
import System.IO qualified as IO
import Test.Hspec
import Test.Hspec.Core.Runner qualified as Spec
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Core.Spec qualified as Spec
import Text.Pretty.Simple (pShowNoColor)

data Config = Config
  { update :: Bool,
    firstTimeUpdate :: Bool,
    filterPred :: Pred
  }

type Pred = FilePath -> Bool

(</&&>) :: Pred -> Pred -> Pred
pred </&&> pred' = (&&) <$> pred <*> pred'

(</||>) :: Pred -> Pred -> Pred
pred </||> pred' = (||) <$> pred <*> pred'

makeFieldLabelsNoPrefix ''Config

spec :: Spec
spec = do
  specWith defConfig

run :: Config -> IO ()
run config =
  Spec.hspecWith
    ( Spec.defaultConfig
        { Spec.configFilterPredicate = Just filterFun
        }
    )
    $ specWith config
  where
    filterFun :: ([FilePath], FilePath) -> Bool
    filterFun (ps, p) = config ^. #filterPred $ FilePath.joinPath ps </> p

specWith :: Config -> Spec
specWith config = do
  llLexerSpec config
  llParserSpec config

llLexerSpec :: Config -> Spec
llLexerSpec config = do
  makeSnapshotSpec
    config
    ( pure
        . LText.toStrict
        . pShowNoColor
        . LL.Lexer.tokenize
    )
    "ll_lexer/ok"

llParserSpec :: Config -> Spec
llParserSpec config = do
  makeSnapshotSpec
    config
    ( \text -> do
        let res = LL.ParserWrapper.parse text LL.Parser.prog
        case res of
          Left es -> Exception.throwString $ LText.unpack $ pShowNoColor es
          Right prog -> pure $ LText.toStrict $ pShowNoColor prog
    )
    "ll_parser/ok"

llCompileSpec :: Config -> Spec
llCompileSpec config = pure ()

-- | cleans .expect files that do not have a matching file
clean :: IO ()
clean = do
  runConduitRes $
    C.sourceDirectoryDeep False "test_data"
      .| C.iterM
        ( \path ->
            liftIO $
              when (FilePath.takeExtension path == ".s") $
                Directory.removeFile path
        )
      .| C.filter (\path -> FilePath.takeExtension path == ".expect")
      .| C.mapM_
        ( \expectPath -> do
            anyExist <-
              runConduit $
                C.yieldMany ((expectPath -<.>) <$> possibleExtensions)
                  .| C.mapM (liftIO . Directory.doesFileExist)
                  .| C.any id
            unless anyExist $ do
              liftIO $ Directory.removeFile expectPath
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
  runEff $
    Driver.runEffs' $
      runReader Opt.defOpt $
        Driver.compileLLFileToAsm
          ("test_data/ll_compile" </> path)
          ("test_data/asm_compile" </> FilePath.takeFileName path -<.> ".s")

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

runAsm :: FilePath -> IO ()
runAsm path = do
  let asmDir = "test_data/asm_compile"
  let exe = FilePath.takeBaseName path
  let exePath = asmDir </> exe
  runEff $
    Process.runProcess $
      runReader Opt.defOpt $ do
        Driver.runEffs' $
          Driver.compileAsmPaths
            [asmDir </> path]
            exePath
        Process.callProcess exePath []

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
      filterPred = const True
    }

makeSnapshotSpec :: Config -> (Text -> IO Text) -> FilePath -> Spec
makeSnapshotSpec config convert dirPath = do
  llCompileOk <- runIOE $ FileSystem.runFileSystem $ FileSystem.listDirectory $ "test_data" </> dirPath
  describe dirPath $
    traverse_
      (makeSnapshotFileSpec config convert dirPath)
      (filter notExpectPath llCompileOk)

notExpectPath :: FilePath -> Bool
notExpectPath path = FilePath.takeExtension path /= ".expect"

makeSnapshotFileSpec :: Config -> (Text -> IO Text) -> FilePath -> FilePath -> Spec
makeSnapshotFileSpec config convert dirPath path = it path $ IOResultSpec example
  where
    fullPath = "test_data" </> dirPath </> path

    example :: IO Spec.Result
    example = do
      contents <- readFileUtf8 fullPath
      let expectPath = fullPath -<.> "expect"
      existsExpect <- Directory.doesFileExist expectPath
      convertContents <- convert contents
      if existsExpect
        then do
          expectContents <- readFileUtf8 expectPath <&> stripLastNewline
          if convertContents /= expectContents
            then
              if config ^. #update
                then do
                  runEff $ writeFileUtf8 expectPath convertContents
                  pure
                    Spec.Result
                      { resultInfo = "Updating the .expect file!",
                        resultStatus = Spec.Success
                      }
                else do
                  runEff $
                    Temporary.runTemporary $
                      Temporary.withSystemTempFile "snap" $ \temp handle -> do
                        hPutUtf8 handle convertContents
                        liftIO $ IO.hFlush handle
                        liftIO $ gitDiff expectPath temp
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
              runEff $ writeFileUtf8 expectPath convertContents
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

gitDiff :: FilePath -> FilePath -> IO ()
gitDiff file file' = do
  runEff $
    Process.runProcess $
      Process.callProcess
        "git"
        [ "diff",
          "--no-index",
          "--color=always",
          file,
          file'
        ]

stripLastNewline :: Text -> Text
stripLastNewline (t :> '\n') = t
stripLastNewline t = t

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = runEff . runErrorIO @UnicodeException . Oat.Common.readFileUtf8

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
