{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Dataflow.Block
  ( Node,
    Block (..),
    BlockK,
    null,
    empty,
    cons,
    snoc,
    append,
    join,
    toList,
    ForwardFold3,
    ForwardFold,
    BackwardFold3,
    foldForward3,
    foldForward,
    foldBackward3,
    foldBackward,
  )
where

import Data.DList (DList)
import Data.DList qualified as DList
import Data.Some (Some1 (..), withSome1)
import GHC.Show qualified as Show
import Oat.Dataflow.Shape (IndexedCO, Shape (..))
import Oat.Utils.Misc ((!>>), (<<!))
import Text.Show qualified as Show
import Prelude hiding (Cons, Empty, Snoc, cons, empty, join, null, snoc, toList)

type Node = Shape -> Shape -> Type

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
toList = DList.toList . go
  where
    go :: forall n e x. Block n e x -> DList (Some1 n)
    go = \case
      CO n b -> Some1 n `DList.cons` go b
      CC n b n' -> Some1 n `DList.cons` (go b `DList.snoc` Some1 n')
      OC b n -> go b `DList.snoc` Some1 n
      Empty -> DList.empty
      Middle n -> DList.singleton $ Some1 n
      Append b b' -> go b `DList.append` go b'
      Snoc b n -> go b `DList.snoc` Some1 n
      Cons n b -> Some1 n `DList.cons` go b

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

type ForwardFold3 n a b c =
  ( n C O -> a -> b,
    n O O -> b -> b,
    n O C -> b -> c
  )

foldForward3 ::
  forall n a b c e x.
  -- 3 folding functions
  ForwardFold3 n a b c ->
  -- the block to fold on
  Block n e x ->
  -- initial accumulator
  IndexedCO e a b ->
  -- final accumulator
  IndexedCO x c b
foldForward3 (foldFirst, foldMiddle, foldLast) = go
  where
    go :: forall e x. Block n e x -> IndexedCO e a b -> IndexedCO x c b
    go = \case
      CO n b -> foldFirst n !>> go b
      CC n b n' -> foldFirst n !>> go b !>> foldLast n'
      OC b n -> go b !>> foldLast n
      Empty -> id
      Middle n -> foldMiddle n
      Append b b' -> go b !>> go b'
      Snoc b n -> go b !>> foldMiddle n
      Cons n b -> foldMiddle n !>> go b

type ForwardFold n a = forall e x. n e x -> a -> a

foldForward :: forall n a e x. ForwardFold n a -> Block n e x -> IndexedCO e a a -> IndexedCO x a a
foldForward f = foldForward3 (f, f, f)

type BackwardFold3 n a b c =
  ( n C O -> b -> c,
    n O O -> b -> b,
    n O C -> a -> b
  )

foldBackward3 ::
  forall n a b c e x.
  BackwardFold3 n a b c ->
  Block n e x ->
  IndexedCO x a b ->
  IndexedCO e c b
foldBackward3 (foldFirst, foldMiddle, foldLast) = go
  where
    go :: forall e x. Block n e x -> IndexedCO x a b -> IndexedCO e c b
    go = \case
      CO n b -> foldFirst n <<! go b
      CC n b n' -> foldFirst n <<! go b <<! foldLast n'
      OC b n -> go b <<! foldLast n
      Empty -> id
      Middle n -> foldMiddle n
      Append b b' -> go b <<! go b'
      Snoc b n -> go b <<! foldMiddle n
      Cons n b -> foldMiddle n <<! go b

type BackwardFold n a = forall e x. n e x -> a -> a

foldBackward :: forall n a e x. BackwardFold n a -> Block n e x -> IndexedCO x a a -> IndexedCO e a a
foldBackward f = foldBackward3 (f, f, f)

type Map3 n n' e x =
  ( n O C -> n' O C,
    n O O -> n' O O,
    n O C -> n' O C
  )

map3 :: forall n n' e x. Map3 n n' e x -> Block n e x -> Block n' e x
map3 (mapFirst, mapMiddle, mapLast) = go
  where
    go :: forall e x. Block n e x -> Block n' e x
    go = \case
      OC b n -> OC (go b) mapLast n
