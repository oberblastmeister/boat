module Oat.Utils.IO
  ( runErrorIO,
    readFileUtf8,
    writeFileUtf8,
    ShowableException (..),
    listDirectory',
    createFileIfMissing,
    hPutUtf8,
  )
where

import Data.ByteString qualified as ByteString
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Oat.Utils.Monad (liftEither, unlessM)
import System.FilePath ((</>))
import System.IO qualified as IO
import UnliftIO.Exception qualified as Exception

readFileUtf8 :: '[Error UnicodeException, IOE] :>> es => FilePath -> Eff es Text
readFileUtf8 path = do
  bs <- liftIO $ ByteString.readFile path
  liftEither $ Text.Encoding.decodeUtf8' bs

writeFileUtf8 :: IOE :> es => FilePath -> Text -> Eff es ()
writeFileUtf8 path = liftIO . ByteString.writeFile path . Text.Encoding.encodeUtf8

hPutUtf8 :: IOE :> es => IO.Handle -> Text -> Eff es ()
hPutUtf8 handle = liftIO . ByteString.hPutStr handle . Text.Encoding.encodeUtf8

listDirectory' :: FileSystem :> es => FilePath -> Eff es [FilePath]
listDirectory' dirPath = do
  paths <- FileSystem.listDirectory dirPath
  pure $ (dirPath </>) <$> paths

createFileIfMissing :: IOE :> es => FilePath -> Eff es ()
createFileIfMissing path = do
  unlessM (FileSystem.runFileSystem $ FileSystem.doesFileExist path) $
    liftIO $ ByteString.writeFile path "\n"

data ShowableException = forall e. Show e => ShowableException e
  deriving (Typeable)

instance Exception.Exception ShowableException

instance Show ShowableException where
  show (ShowableException e) = show e

runErrorIO :: forall e es a. (IOE :> es, Show e) => Eff (Error e ': es) a -> Eff es a
runErrorIO m = do
  res <- Error.runError m
  case res of
    Left e -> Exception.throwIO $ ShowableException e
    Right a -> pure a
