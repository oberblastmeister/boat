{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Fuel
  ( Fuel (..),
    hasFuel,
    useFuel,
  )
where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.Map qualified as M
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (CallStack, Error, runError, throwError)
import Effectful.State.Static.Local
import System.IO qualified as IO
import Prelude hiding (Filesystem, readFile)

data Fuel :: Effect where
  HasFuel :: Fuel m Bool
  UseFuel :: Fuel m ()

type instance DispatchOf Fuel = 'Dynamic

hasFuel :: Fuel :> es => Eff es Bool
hasFuel = send HasFuel

useFuel :: Fuel :> es => Eff es ()
useFuel = send UseFuel

-- * Effect

-- | An effect for reading and writing files.
data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = 'Dynamic

readFile :: FileSystem :> es => FilePath -> Eff es String
readFile = send . ReadFile

-- * Operations

-- * Interpretations

-- | File system error.
newtype FsError = FsError String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Run the 'Filesystem' effect with actual file IO.
-- runFilesystemIO :: '[IOE, Error FsError] :>> es => Eff (Filesystem ': es) a -> Eff es a
-- runFilesystemIO = interpret $ \case
--   ReadFile path -> adapt $ IO.readFile path
--   WriteFile path contents -> adapt $ IO.writeFile path contents
--   where
--     adapt m = liftIO m `Exception.catch` \(e :: Exception.IOException) -> throwError $ FsError $ show e

-- | Run the 'Filesystem' effect with a faked filesystem.
runFileSystemPure ::
  Error FsError :> es =>
  M.Map FilePath String ->
  Eff (FileSystem : es) a ->
  Eff es a
runFileSystemPure fs0 = reinterpret (evalState fs0) $ \_ -> \case
  ReadFile path ->
    gets (M.lookup path) >>= \case
      Just contents -> pure contents
      Nothing -> throwError . FsError $ "File not found: " ++ show path
  WriteFile path contents -> modify $ M.insert path contents

maybeM :: Eff es0 a2 -> (a3 -> f0 a3) -> Eff es1 (Maybe a4) -> FileSystem (Eff localEs) a5 -> Eff (State (Map FilePath String) : es) a5
maybeM = error "not implemented"

runFileSystemPure' :: M.Map FilePath String -> Eff (FileSystem ': Error FsError ': es) a -> Eff es (Either (CallStack, FsError) a)
runFileSystemPure' fs = runFileSystemPure fs >>> runError @FsError

-- f :: Either FsError (Either (CallStack, FsError) String)
-- f :: Either (CallStack, FsError) (Either (CallStack, FsError) String)

-- interpret
--   :: DispatchOf e ~ 'Dynamic
--   => EffectHandler e es
--   -- ^ The effect handler.
--   -> Eff (e : es) a
--   -> Eff      es  a
-- interpret
--   :: (DispatchOf e ~ 'Dynamic)
--   => EffectHandler e es
--   -- ^ The effect handler.
--   -> Eff (e : es) a
--   -> Eff      es  a
