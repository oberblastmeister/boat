{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Source
  ( Source,
    HasSource,
    MonadSource,
    new,
    fresh,
  )
where

import Data.Infinite (Infinite)
import Data.Infinite qualified as Infinite

newtype Source a = Source {source :: Infinite a}
  deriving (Functor)

makeFieldLabelsNoPrefix ''Source

type HasSource s a = LabelOptic' "source" A_Lens s (Source a)

type MonadSource s m a =
  ( MonadState s m,
    HasSource s a
  )

new :: Infinite a -> Source a
new = Source

fresh :: MonadSource s m a => m a
fresh = do
  as <- use (#source % #source)
  #source % #source .= as
  pure $ Infinite.head as