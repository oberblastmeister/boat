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
    (><),
    join,
    toList,
    ForwardFold3,
    ForwardFold3M,
    ForwardFold,
    BackwardFold3,
    BackwardFold3M,
    foldForward3,
    foldForward,
    foldBackward3,
    foldBackward,
    map3,
    foldBackward3M,
    foldForward3M,
    Map3M,
    map3M,
    map,
  )
where

import Data.DList (DList)
import Data.DList qualified as DList
import Data.Some (Some1 (..), withSome1)
import GHC.Show qualified as Show
import Oat.Dataflow.Shape
  ( IndexedCO,
    KnownShape (shape),
    Shape (..),
    Shape' (..),
    mapIndexedCO,
  )
import Oat.Utils.Misc ((!>>), (<<!))
import Text.Show qualified as Show
import Prelude hiding (Cons, Empty, Snoc, cons, empty, join, map, null, snoc, toList)

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
  Single :: !(n O O) -> Block n O O
  Append :: !(Block n O O) -> !(Block n O O) -> Block n O O

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
      Single n -> DList.singleton $ Some1 n
      Append b b' -> go b <> go b'

null :: Block n e x -> Bool
null Empty = True
null (Append b b') = null b && null b'
null _ = False

empty :: Block n O O
empty = Empty

class Cons e where
  cons :: forall n x. n e O -> Block n O x -> Block n e x

instance Cons O where
  cons n b = Single n `append` b

instance Cons C where
  cons f (OC b l) = CC f b l
  cons f b = CO f Empty `append` b

class Snoc x where
  snoc :: forall n e. Block n e O -> n O x -> Block n e x

instance Snoc O where
  snoc b n = b `append` Single n

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
    Single {} -> CO l (b `append` y)
    Append {} -> CO l (b `append` y)
  Single {} -> case y of
    OC b n' -> OC (x `append` b) n'
    Empty -> x
    Single {} -> x `Append` y
    Append {} -> x `Append` y
  Append {} -> case y of
    OC b n -> OC (x `append` b) n
    Empty -> x
    Single {} -> x `Append` y
    Append {} -> x `Append` y

(><) :: Block n e 'O -> Block n 'O x -> Block n e x
(><) = append

infixl 5 ><

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
foldForward3 (foldFirst, foldMiddle, foldLast) = \case
  CO n b -> foldFirst n >>> go b
  CC n b n' -> foldFirst n >>> go b !>> foldLast n'
  OC b n -> go b !>> foldLast n
  Empty -> id
  Single n -> foldMiddle n
  b@Append {} -> go b
  where
    -- rebalance to the right while folding from the left
    -- this ensures that we get tail calls
    go :: Block n O O -> b -> b
    go =
      id !>> \case
        Empty -> id
        Single n -> foldMiddle n
        Append b b' -> goAppend b b'
      where
        goAppend :: Block n O O -> Block n O O -> b -> b
        goAppend a b = case a of
          Empty -> go b
          Single n -> foldMiddle n >>> go b
          Append c d -> goAppend c (Append d b)

type ForwardFold3M n a b c es =
  ( n C O -> a -> Eff es b,
    n O O -> b -> Eff es b,
    n O C -> b -> Eff es c
  )

foldForward3M ::
  forall n a b c e x es.
  (KnownShape e, KnownShape x) =>
  ForwardFold3M n a b c es ->
  Block n e x ->
  IndexedCO e a b ->
  Eff es (IndexedCO x c b)
foldForward3M (foldFirst, foldMiddle, foldLast) block indexed =
  case shape @x of
    O' -> res
    C' -> res
  where
    res = foldForward3 (foldFirst', foldMiddle', foldLast') block indexed'

    indexed' :: IndexedCO e (Eff es a) (Eff es b)
    indexed' =
      ( mapIndexedCO
          (Proxy @e)
          (pure @(Eff es) @a)
          (pure @(Eff es) @b)
          indexed
      )

    foldFirst' :: n C O -> Eff es a -> Eff es b
    foldFirst' n a = a >>= (foldFirst n $!)

    foldMiddle' :: n O O -> Eff es b -> Eff es b
    foldMiddle' n b = b >>= (foldMiddle n $!)

    foldLast' :: n O C -> Eff es b -> Eff es c
    foldLast' n b = b >>= (foldLast n $!)

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
foldBackward3 (foldFirst, foldMiddle, foldLast) = \case
  CO n b -> foldFirst n <<! go b
  CC n b n' -> foldFirst n <<! go b <<< foldLast n'
  OC b n -> go b <<< foldLast n
  Empty -> id
  Single n -> foldMiddle n
  b@Append {} -> go b
  where
    -- rebalance to the left while folding from the right
    -- this ensures that we get tail calls
    go :: Block n O O -> b -> b
    go =
      id !>> \case
        Empty -> id
        Single n -> foldMiddle n
        Append b b' -> goAppend b b'
      where
        goAppend :: Block n O O -> Block n O O -> b -> b
        goAppend a b = case b of
          Empty -> go a
          Single n -> go a <<< foldMiddle n
          Append c d -> goAppend (Append a c) d

type BackwardFold n a = forall e x. n e x -> a -> a

foldBackward :: forall n a e x. BackwardFold n a -> Block n e x -> IndexedCO x a a -> IndexedCO e a a
foldBackward f = foldBackward3 (f, f, f)

type BackwardFold3M n a b c es =
  ( n C O -> b -> Eff es c,
    n O O -> b -> Eff es b,
    n O C -> a -> Eff es b
  )

foldBackward3M ::
  forall n a b c e x es.
  (KnownShape e, KnownShape x) =>
  BackwardFold3M n a b c es ->
  Block n e x ->
  IndexedCO x a b ->
  Eff es (IndexedCO e c b)
foldBackward3M (foldFirst, foldMiddle, foldLast) block indexed =
  case shape @e of
    O' -> res
    C' -> res
  where
    res = foldBackward3 (foldFirst', foldMiddle', foldLast') block indexed'

    indexed' :: IndexedCO x (Eff es a) (Eff es b)
    indexed' =
      ( mapIndexedCO
          (Proxy @x)
          (pure @(Eff es) @a)
          (pure @(Eff es) @b)
          indexed
      )

    foldFirst' :: n C O -> Eff es b -> Eff es c
    foldFirst' n a = a >>= (foldFirst n $!)

    foldMiddle' :: n O O -> Eff es b -> Eff es b
    foldMiddle' n b = b >>= (foldMiddle n $!)

    foldLast' :: n O C -> Eff es a -> Eff es b
    foldLast' n b = b >>= (foldLast n $!)

type Map3 n n' e x =
  ( n C O -> n' C O,
    n O O -> n' O O,
    n O C -> n' O C
  )

map3 :: forall n n' e x. Map3 n n' e x -> Block n e x -> Block n' e x
map3 (mapFirst, mapMiddle, mapLast) = go
  where
    go :: forall e x. Block n e x -> Block n' e x
    go = \case
      CO n b -> CO (mapFirst n) (go b)
      CC n b n' -> CC (mapFirst n) (go b) (mapLast n')
      OC b n -> OC (go b) (mapLast n)
      Empty -> Empty
      Single n -> Single (mapMiddle n)
      Append b b' -> Append (go b) (go b')

map ::
  forall n n' e x.
  (forall e x. n e x -> n' e x) ->
  Block n e x ->
  Block n' e x
map f = map3 (f, f, f)

type Map3M n n' e x es =
  ( n C O -> Eff es (n' C O),
    n O O -> Eff es (n' O O),
    n O C -> Eff es (n' O C)
  )

map3M :: forall n n' e x es. Map3M n n' e x es -> Block n e x -> Eff es (Block n' e x)
map3M (mapFirst, mapMiddle, mapLast) = go
  where
    go :: forall e x. Block n e x -> Eff es (Block n' e x)
    go = \case
      CO n b -> CO <$> mapFirst n <*> go b
      CC n b n' -> CC <$> mapFirst n <*> go b <*> mapLast n'
      OC b n -> OC <$> go b <*> mapLast n
      Empty -> pure Empty
      Single n -> Single <$> mapMiddle n
      Append b b' -> Append <$> go b <*> (go b')
