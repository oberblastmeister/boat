{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.IdGen
  ( IdGen,
    HasIdGen (..),
    new,
    freshId,
    freshTemp,
  )
where

import Data.ASCII (ASCII, unsafeToASCII)

newtype IdGen = IdGen {currId :: Int}

makeFieldLabelsNoPrefix ''IdGen

class HasIdGen a b | a -> b where
  idGen :: Lens' a b

new :: IdGen
new = IdGen {currId = 0}

freshId :: (MonadState a m, HasIdGen a IdGen) => m Int
freshId = do
  id <- use (idGen % #currId)
  idGen % #currId %= (+ 1)
  pure id

freshTemp :: (MonadState a m, HasIdGen a IdGen) => m (ASCII ByteString)
freshTemp = do
  id <- freshId
  pure $ unsafeToASCII $ fromString $ show id