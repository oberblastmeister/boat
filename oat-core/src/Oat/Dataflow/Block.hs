{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Dataflow.Block
  ( Node,
    Shape (..),
    Shape' (..),
    IndexedCO,
    MaybeO (..),
    MaybeC (..),
    Block (..),
    BlockK,
    Some1 (..),
    null,
    empty,
    cons,
    snoc,
    append,
    join,
    toList,
  )
where

import Data.DList (DList)
import Data.DList qualified as DL
import Data.Some (Some1 (..), withSome1)
import GHC.Show qualified as Show
import Text.Show qualified as Show
import Prelude hiding (Cons, Empty, Snoc, cons, empty, join, null, snoc, toList)

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
data Shape' :: Shape -> Type where
  O' :: Shape' O
  C' :: Shape' C

type IndexedCO :: Shape -> a -> a -> a
type family IndexedCO ex a b

type instance IndexedCO C a _b = a

type instance IndexedCO O _a b = b

-- | Maybe type indexed by open/closed
data MaybeO :: Shape -> Type -> Type where
  JustO :: !t -> MaybeO O t
  NothingO :: MaybeO C t

deriving instance (Show a) => Show (MaybeO ex a)

deriving instance (Eq a) => Eq (MaybeO ex a)

deriving instance Functor (MaybeO ex)

-- | Maybe type indexed by closed/open
data MaybeC :: Shape -> Type -> Type where
  JustC :: !t -> MaybeC C t
  NothingC :: MaybeC O t

deriving instance (Show a) => Show (MaybeC ex a)

deriving instance (Eq a) => Eq (MaybeC ex a)

deriving instance Functor (MaybeC ex)

type BlockK = Node -> Shape -> Shape -> Type

-- | A sequence of nodes.  May be any of four shapes (O/O, O/C, C/O, C/C).
-- Open at the entry means single entry, mutatis mutandis for exit.
-- A closed/closed block is a /basic/ block and can't be extended further.
-- Clients should avoid manipulating blocks and should stick to either nodes
-- or graphs.
data Block :: BlockK where
  CO :: !(n C O) -> !(Block n O O) -> Block n C O
  CC :: !(n C O) -> !(Block n O O) -> !(n O C) -> Block n C C
  OC :: !(Block n O O) -> !(n O C) -> Block n O C
  Empty :: Block n O O
  Middle :: !(n O O) -> Block n O O
  Append :: !(Block n O O) -> !(Block n O O) -> Block n O O
  Snoc :: !(Block n O O) -> !(n O O) -> Block n O O
  Cons :: !(n O O) -> !(Block n O O) -> Block n O O

instance (forall e x. (Show (n e x))) => (Show (Block n e x)) where
  showsPrec d block =
    showParen (d > Show.appPrec) $
      ( \l ->
          (Show.showString "Block")
            . Show.showListWith (withSome1 shows) l
      )
        $ toList block

deriving instance (forall e x. Eq (n e x)) => (Eq (Block n e x))

toList :: forall n e x. Block n e x -> [Some1 n]
toList = DL.toList . go
  where
    go :: forall n e x. Block n e x -> DList (Some1 n)
    go = \case
      CO n b -> Some1 n `DL.cons` go b
      CC n b n' -> Some1 n `DL.cons` (go b `DL.snoc` Some1 n')
      OC b n -> go b `DL.snoc` Some1 n
      Empty -> DL.empty
      Middle n -> DL.singleton $ Some1 n
      Append b b' -> go b `DL.append` go b'
      Snoc b n -> go b `DL.snoc` Some1 n
      Cons n b -> Some1 n `DL.cons` go b

null :: Block n e x -> Bool
null Empty = True
null (Append b b') = null b && null b'
null _ = False

empty :: Block n O O
empty = Empty

class Cons e where
  cons :: forall n x. n e O -> Block n O x -> Block n e x

instance Cons O where
  cons n b = case b of
    OC b l -> OC (cons n b) l
    Empty -> Middle n
    Middle {} -> n `Cons` b
    Append {} -> n `Cons` b
    Snoc {} -> n `Cons` b
    Cons {} -> n `Cons` b

instance Cons C where
  cons f (OC b l) = CC f b l
  cons f b = CO f Empty `append` b

class Snoc x where
  snoc :: forall n e. Block n e O -> n O x -> Block n e x

instance Snoc O where
  snoc b n = case b of
    CO f b -> CO f (b `snoc` n)
    Empty -> Middle n
    Middle {} -> b `Snoc` n
    Append {} -> b `Snoc` n
    Snoc {} -> b `Snoc` n
    Cons {} -> b `Snoc` n

instance Snoc C where
  snoc (CO f b) t = CC f b t
  snoc b t = b `append` OC Empty t

join :: n C O -> Block n O O -> n O C -> Block n C C
join f b t = CC f b t

append :: Block n e O -> Block n O x -> Block n e x
append x y = case x of
  Empty -> y
  CO l b -> case y of
    OC b' n -> CC l (b `append` b') n
    Empty -> x
    Middle {} -> CO l (b `append` y)
    Append {} -> CO l (b `append` y)
    Snoc {} -> CO l (b `append` y)
    Cons {} -> CO l (b `append` y)
  Middle n -> case y of
    OC b n' -> OC (x `append` b) n'
    Empty -> x
    Middle {} -> n `Cons` y
    Append {} -> n `Cons` y
    Snoc {} -> n `Cons` y
    Cons {} -> n `Cons` y
  Append {} -> case y of
    OC b n -> OC (x `append` b) n
    Empty -> x
    Middle n -> x `Snoc` n
    Append {} -> x `Append` y
    Snoc {} -> x `Append` y
    Cons {} -> x `Append` y
  Snoc {} -> case y of
    OC b n -> OC (x `append` b) n
    Empty -> x
    Middle n -> x `Snoc` n
    Append {} -> x `Append` y
    Snoc {} -> x `Append` y
    Cons {} -> x `Append` y
  Cons {} -> case y of
    OC b n -> OC (x `append` b) n
    Empty -> x
    Middle n -> x `Snoc` n
    Append {} -> x `Append` y
    Snoc {} -> x `Append` y
    Cons {} -> x `Append` y
