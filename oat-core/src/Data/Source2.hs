{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Source
  ( Source,
    HasSource,
    MonadSource,
    new,
  )
where

import Data.Infinite (Infinite)
import Data.Infinite qualified as Infinite

newtype Source a = Source {source :: Infinite a}
  deriving (Functor)

makeFieldLabelsNoPrefix ''Source

type Flip c b a = c a b

class HasSource a s | s -> a where
  source :: Lens' s a

newtype LabelOpticHasSource a = LabelOpticHasSource a

instance
  ( nt ~ LabelOpticHasSource a,
    src ~ Source b,
    LabelOptic "source" A_Lens nt nt src src
  ) =>
  HasSource src nt
  where
  source = #source

type MonadSource s m a =
  ( MonadState s m,
    HasSource s a
  )

new :: Infinite a -> Source a
new = Source