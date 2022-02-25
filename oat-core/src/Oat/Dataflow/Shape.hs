{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Dataflow.Shape
  ( Shape (..),
    Shape' (..),
    IndexedCO,
    MaybeO (..),
    MaybeC (..),
  )
where

-- | Shapes: Open and Closed
data Shape
  = -- | Used at the type level to indicate an "open" structure with
    -- a unique, unnamed control-flow edge flowing in or out.
    -- "Fallthrough" and concatenation are permitted at an open point.
    O
  | -- | Used at the type level to indicate a "closed" structure which
    -- supports control transfer only through the use of named
    -- labels---no "fallthrough" is permitted.  The number of control-flow
    -- edges is unconstrained.
    C

-- | Shape singleton. Get evidence of the shape by pattern matching.
data Shape' :: Shape -> Type where
  O' :: Shape' O
  C' :: Shape' C

type IndexedCO :: Shape -> a -> a -> a
type family IndexedCO ex a b

type instance IndexedCO C a _ = a

type instance IndexedCO O _ b = b

-- | Maybe type indexed by open/closed
data MaybeO :: Shape -> Type -> Type where
  JustO :: !t -> MaybeO O t
  NothingO :: MaybeO C t

deriving instance (Show a) => Show (MaybeO ex a)

deriving instance (Eq a) => Eq (MaybeO ex a)

deriving instance Functor (MaybeO ex)

deriving instance Foldable (MaybeO ex)

deriving instance Traversable (MaybeO ex)

-- | Maybe type indexed by closed/open
data MaybeC :: Shape -> Type -> Type where
  JustC :: !t -> MaybeC C t
  NothingC :: MaybeC O t

deriving instance (Show a) => Show (MaybeC ex a)

deriving instance (Eq a) => Eq (MaybeC ex a)

deriving instance Functor (MaybeC ex)

deriving instance Foldable (MaybeC ex)

deriving instance Traversable (MaybeC ex)
