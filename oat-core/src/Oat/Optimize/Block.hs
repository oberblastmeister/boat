{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Optimize.Block where

type Node = Shape -> Shape -> Type

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
data SShape :: Shape -> Type where
  SO :: SShape O
  SC :: SShape C

type IndexedCO :: Shape -> a -> a -> a
type family IndexedCO ex a b

type instance IndexedCO C a _b = a

type instance IndexedCO O _a b = b

-- | Maybe type indexed by open/closed
data MaybeO :: Shape -> Type -> Type where
  JustO :: t -> MaybeO O t
  NothingO :: MaybeO C t

deriving instance (Show a) => Show (MaybeO ex a)

deriving instance (Eq a) => Eq (MaybeO ex a)

deriving instance Functor (MaybeO ex)

-- | Maybe type indexed by closed/open
data MaybeC :: Shape -> Type -> Type where
  JustC :: t -> MaybeC C t
  NothingC :: MaybeC O t

deriving instance (Show a) => Show (MaybeC ex a)

deriving instance (Eq a) => Eq (MaybeC ex a)

deriving instance Functor (MaybeC ex)

-- | A sequence of nodes.  May be any of four shapes (O/O, O/C, C/O, C/C).
-- Open at the entry means single entry, mutatis mutandis for exit.
-- A closed/closed block is a /basic/ block and can't be extended further.
-- Clients should avoid manipulating blocks and should stick to either nodes
-- or graphs.
data Block :: Node -> Shape -> Shape -> Type where
  BlockCO :: n C O -> Block n O O -> Block n C O
  BlockCC :: n C O -> Block n O O -> n O C -> Block n C C
  BlockOC :: Block n O O -> n O C -> Block n O C
  BNil :: Block n O O
  BMiddle :: n O O -> Block n O O
  BCat :: Block n O O -> Block n O O -> Block n O O
  BSnoc :: Block n O O -> n O O -> Block n O O
  BCons :: n O O -> Block n O O -> Block n O O

deriving instance (forall e x. Show (n e x)) => (Show (Block n e x))

deriving instance (forall e x. Eq (n e x)) => (Eq (Block n e x))
