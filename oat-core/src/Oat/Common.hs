module Oat.Common
  ( internalError,
    unwrap,
    unreachable,
    hashSetOf,
    inBetween,
    ShowableException (..),
    runErrorIO,
    liftEither,
    readFileUtf8,
    writeFileUtf8,
    listDirectory',
    createFileIfMissing,
    hPutUtf8,
    pathTail,
    concatToEither,
    maybeToRight,
    maybeToLeft,
    rightToMaybe,
    leftToMaybe,
    mVecFromList,
    timSortListBy,
    timSortNEBy,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.ByteString qualified as ByteString
import Data.HashSet qualified as HashSet
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Range (Range (RangeP))
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Data.Vector qualified as VB
import Data.Vector.Algorithms.Tim qualified as Vector.Algorithms.Tim
import Data.Vector.Mutable qualified as VBM
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Oat.Utils.Monad (unlessM)
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO qualified as IO
import UnliftIO.Exception qualified as Exception
import Prelude hiding (Map, imap)

internalError :: forall a. HasCallStack => Text -> a
internalError t = error $ "Internal compiler error: " <> t

-- non but only a lens
unwrap :: a -> Lens' (Maybe a) a
unwrap def =
  lens
    (fromMaybe def)
    ( \m b -> case m of
        Just _ -> Just b
        Nothing -> Just def
    )

unreachable :: forall a. a
unreachable = error "Unreachable!"

hashSetOf :: (Eq a, Hashable a, Is k A_Fold) => Optic' k is s a -> s -> HashSet a
hashSetOf o = foldMapOf o HashSet.singleton

inBetween :: Range -> IntMap a -> IntMap a
inBetween (RangeP start end) imap = imap''
  where
    (imap'', _) = IntMap.split end imap'
    (_, imap') = IntMap.split start imap

data ShowableException = forall e. Show e => ShowableException e
  deriving (Typeable)

instance Exception.Exception ShowableException

instance Show ShowableException where
  show (ShowableException e) = show e

runErrorIO :: forall e es a. (IOE :> es, Show e) => Eff (Error e ': es) a -> Eff es a
runErrorIO m = do
  res <- runError m
  case res of
    Left e -> Exception.throwIO $ ShowableException e
    Right a -> pure a

liftEither :: (Error e :> es) => Either e a -> Eff es a
liftEither (Left e) = throwError e
liftEither (Right a) = pure a

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

pathTail :: FilePath -> FilePath
pathTail = FilePath.joinPath . tail . FilePath.splitPath

concatToEither :: [a] -> [b] -> [Either a b]
concatToEither as bs = fmap Left as ++ fmap Right bs

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just a) = Left a
maybeToLeft b Nothing = Right b

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just b) = Right b
maybeToRight a Nothing = Left a

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

-- this is save because the created vector will never be used
mVecFromList :: PrimMonad m => [a] -> m (VBM.MVector (PrimState m) a)
mVecFromList = VB.unsafeThaw . VB.fromList

timSortListBy :: (a -> a -> Ordering) -> [a] -> [a]
timSortListBy cmp as = VB.toList $
  VB.create $ do
    mv <- mVecFromList as
    Vector.Algorithms.Tim.sortBy cmp mv
    pure mv

timSortNEBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
timSortNEBy cmp = NonEmpty.fromList . timSortListBy cmp . NonEmpty.toList
