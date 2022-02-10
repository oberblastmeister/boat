{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Common
  ( internalError,
    unwrap,
    unreachable,
    hashSetOf,
    inBetween,
    swap,
    ShowableException,
    runErrorIO,
    liftEither,
    readFileUtf8,
    writeFileUtf8,
    parOver,
    evalOf,
    parOf,
    listDirectory',
    unlessM,
    createFileIfMissing,
    hPutUtf8,
    pathTail,
  whenM)
where

import Control.Exception.Safe qualified as Exception
import Control.Parallel.Strategies qualified as Parallel
import Data.ByteString qualified as ByteString
import Data.HashSet qualified as HashSet
import Data.IntMap qualified as IntMap
import Data.Range (Range (RangeP))
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO qualified as IO
import Prelude hiding (Map)

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
hashSetOf fold = foldMapOf fold HashSet.singleton

inBetween :: Range -> IntMap a -> IntMap a
inBetween (RangeP start end) imap = imap''
  where
    (imap'', _) = IntMap.split end imap'
    (_, imap') = IntMap.split start imap

swap :: (Is k An_AffineFold, Is k' A_Review) => Optic' k is t a -> Optic' k' is' t b -> Setter t t a b
swap o o' = sets $ \f x -> case x ^? o of
  Nothing -> x
  Just y -> o' # f y
{-# INLINE swap #-}

data ShowableException = forall e. Show e => ShowableException e
  deriving (Typeable)

instance Exception.Exception ShowableException

instance Show ShowableException where
  show (ShowableException e) = show e

runErrorIO :: forall e es a. (Show e) => Eff (Error e ': es) a -> Eff es a
runErrorIO m = do
  res <- runError m
  case res of
    Left e -> Exception.throw $ ShowableException e
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

parOver :: Is k A_Traversal => Parallel.Strategy b -> Optic k is s t a b -> (a -> b) -> s -> t
parOver strat o f = Parallel.runEval . traverseOf o (Parallel.rparWith strat . f)
{-# INLINE parOver #-}

evalOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
evalOf = traverseOf
{-# INLINE evalOf #-}

parOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
parOf o strat = traverseOf o $ Parallel.rparWith strat
{-# INLINE parOf #-}

listDirectory' :: FileSystem :> es => FilePath -> Eff es [FilePath]
listDirectory' dirPath = do
  paths <- FileSystem.listDirectory dirPath
  pure $ (dirPath </>) <$> paths

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM m = do
  cond <- condM
  unless cond m
{-# INLINE unlessM #-}

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM m = do
  cond <- condM
  when cond m
{-# INLINE whenM #-}

createFileIfMissing :: IOE :> es => FilePath -> Eff es ()
createFileIfMissing path = do
  unlessM (FileSystem.runFileSystem $ FileSystem.doesFileExist path) $
    liftIO $ ByteString.writeFile path "\n"

pathTail :: FilePath -> FilePath
pathTail = FilePath.joinPath . tail . FilePath.splitPath
