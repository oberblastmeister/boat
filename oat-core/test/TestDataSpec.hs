module TestDataSpec where

import Data.Foldable (traverse_)
import Data.IORef qualified as IORef
import Data.Text.Lazy qualified as LText
import Effectful.FileSystem qualified as FileSystem
import Effectful.Process qualified as Process
import Effectful.Temporary qualified as Temporary
import Oat.Common (hPutUtf8, listDirectory', writeFileUtf8)
import Oat.Common qualified as Oat.Common
import Oat.LL.Lexer qualified as LL.Lexer
import System.Directory qualified as Directory
import System.FilePath ((-<.>), (</>))
import System.FilePath qualified as FilePath
import System.IO qualified as IO
import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Core.Spec qualified as Spec
import Text.Pretty.Simple (pShowNoColor)

spec :: Spec
spec = do
  llLexerSpec

llLexerSpec :: Spec
llLexerSpec = do
  makeSnapshotSpec
    "ll_lexer/ok"
    ( pure
        . LText.toStrict
        . pShowNoColor
        . LL.Lexer.tokenize
    )

makeSnapshotSpec :: FilePath -> (Text -> IO Text) -> Spec
makeSnapshotSpec dirPath convert = do
  llCompileOk <- runIOE $ FileSystem.runFileSystem $ listDirectory' $ "test_data" </> dirPath
  describe dirPath $
    traverse_
      (`makeSnapshotFileSpec` convert)
      ( filter
          (\path -> FilePath.takeExtension path /= ".expect")
          llCompileOk
      )

makeSnapshotFileSpec :: FilePath -> (Text -> IO Text) -> Spec
makeSnapshotFileSpec path convert = do
  contents <- readFileUtf8 path
  let expectPath = path -<.> "expect"
  existsExpect <- runIO $ Directory.doesFileExist expectPath
  if existsExpect
    then do
      expectContents <- readFileUtf8 expectPath <&> stripLastNewline
      it path $
        IOResultSpec $ do
          convertContents <- convert contents
          if convertContents /= expectContents
            then do
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
    else do
      it path $
        IOResultSpec $ do
          expectConverted <- convert contents
          runEff $ writeFileUtf8 expectPath expectConverted
          pure
            Spec.Result
              { resultInfo = "First time running. Updating the .expect file!",
                resultStatus = Spec.Success
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

readFileUtf8 :: FilePath -> SpecM arg Text
readFileUtf8 = runIO . runEff . Oat.Common.runErrorIO . Oat.Common.readFileUtf8

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
