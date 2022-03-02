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
    map3,
    map3M,
    map,
    singleton,
  )
where

import Acc (Acc)
import Acc qualified as Acc
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Some (Some1 (..), withSome1)
import GHC.Exts qualified
import GHC.Show qualified as Show
import Oat.Dataflow.Shape (Shape (..))
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
  OO :: !(Acc (n O O)) -> Block n O O

instance (forall e x. (Show (n e x))) => (Show (Block n e x)) where
  showsPrec d block =
    showParen (d > Show.appPrec) $
      ( \l ->
          (Show.showString "Block")
            . Show.showListWith (withSome1 shows) l
      )
        $ toList block

toList :: forall n e x. Block n e x -> [Some1 n]
toList = DList.toList . go
  where
    go :: forall n e x. Block n e x -> DList (Some1 n)
    go = \case
      CO n b -> Some1 n `DList.cons` go b
      CC n b n' -> Some1 n `DList.cons` (go b `DList.snoc` Some1 n')
      OC b n -> go b `DList.snoc` Some1 n
      OO bs -> fromList @(DList _) $ Some1 <$> GHC.Exts.toList @(Acc _) bs

null :: Block n e x -> Bool
null (OO (Acc.toNonEmpty -> Nothing)) = True
null _ = False

empty :: Block n O O
empty = OO mempty

class Cons e where
  cons :: forall n x. n e O -> Block n O x -> Block n e x

instance Cons O where
  cons n b = singleton n `append` b

instance Cons C where
  cons f (OC b l) = CC f b l
  cons f b = CO f empty `append` b

class Snoc x where
  snoc :: forall n e. Block n e O -> n O x -> Block n e x

instance Snoc O where
  snoc b n = b `append` (OO $ pure n)

instance Snoc C where
  snoc (CO f b) t = CC f b t
  snoc b t = b `append` OC empty t

join :: n C O -> Block n O O -> n O C -> Block n C C
join f b t = CC f b t

append :: Block n e O -> Block n O x -> Block n e x
append x y = case x of
  CO l (OO b) -> case y of
    OC (OO b') n -> CC l (OO $ b <> b') n
    OO bs -> CO l (OO $ b <> bs)
  OO bs -> case y of
    OO bs' -> OO $ bs <> bs'
    OC (OO b) n -> OC (OO $ bs <> b) n

singleton :: n 'O 'O -> Block n 'O 'O
singleton = OO . pure

(><) :: Block n e 'O -> Block n 'O x -> Block n e x
(><) = append

infixl 5 ><

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
      OO bs -> OO (mapMiddle <$> bs)

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
      OO bs -> OO <$> traverse mapMiddle bs
