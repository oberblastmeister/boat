{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Rec
  ( Rec,
    Key (..),
    empty,
    insert,
    lookup,
    update,
    remove,
  )
where

import Data.Vector qualified as VB
import Fcf hiding (Any)
import Fcf.Class.Bifunctor (Second)
import Fcf.Data.List
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal)
import Unsafe.Coerce qualified
import Prelude hiding (empty, lookup)

type Rec :: (k -> Type) -> [(Symbol, k)] -> Type
data Rec f ts where
  Rec :: {-# UNPACK #-} !(VB.Vector Any) -> Rec f ts

instance
  ( is ~ NoIx,
    k ~ A_Lens,
    KnownNat (FindElem name ts),
    t ~ Eval (LookupType name ts),
    ts' ~ Eval (UpdateElem name t' ts)
  ) =>
  IsLabel name (Optic k is (Rec f ts) (Rec f ts') (f t) (f t'))
  where
  fromLabel = lens (lookup (Key @name)) (flip $ update $ Key @name)

type Key :: Symbol -> Type
data Key key = Key

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

empty :: Rec f '[]
empty = Rec VB.empty

insert ::
  Eval (UniqueKey key ts) ~ 'True =>
  Key key ->
  f t ->
  Rec f ts ->
  Rec f ('(key, t) ': ts)
insert _ ft (Rec v) = Rec $ VB.cons (Unsafe.Coerce.unsafeCoerce ft) v

type UniqueKey :: k -> [(k, t)] -> Exp Bool
type UniqueKey key ts = Null =<< Filter (TyEq key <=< Fst) ts

type FindElem :: Symbol -> [(Symbol, k)] -> Nat
type FindElem key ts = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType :: k -> [(k, t)] -> Exp t
type LookupType key ts = FromMaybe Stuck =<< Lookup key ts

lookup :: forall key ts f. KnownNat (FindElem key ts) => Key key -> Rec f ts -> f (Eval (LookupType key ts))
lookup _ (Rec v) = Unsafe.Coerce.unsafeCoerce $ VB.unsafeIndex v $ findElem @key @ts

type UpdateElem :: Symbol -> k -> [(Symbol, k)] -> Exp [(Symbol, k)]
type UpdateElem key t ts = SetIndex (FindElem key ts) '(key, t) ts

update :: forall key ts t f. KnownNat (FindElem key ts) => Key key -> f t -> Rec f ts -> Rec f (Eval (UpdateElem key t ts))
update _ ft (Rec v) = Rec $ v VB.// [(findElem @key @ts, Unsafe.Coerce.unsafeCoerce ft)]

data AppendTuple :: ([a], [a]) -> Exp [a]

type instance Eval (AppendTuple '(as, as')) = Eval (as ++ as')

type SplitAt_ :: Nat -> [a] -> ([a], [a])
type family SplitAt_ n xs where
  SplitAt_ n xs = '(Eval (Take n xs), Eval (Drop n xs))

data SplitAt :: Nat -> [a] -> Exp ([a], [a])

type instance Eval (SplitAt n xs) = SplitAt_ n xs

type RemoveAt_ :: Nat -> [a] -> [a]
type family RemoveAt_ n xs where
  RemoveAt_ n xs = Eval (AppendTuple =<< (Second (FromMaybe Stuck <=< Tail)) =<< SplitAt n xs)

data RemoveAt :: Nat -> [k] -> Exp [k]

type instance Eval (RemoveAt n xs) = RemoveAt_ n xs

type RemoveElem :: Symbol -> [(Symbol, k)] -> Exp [(Symbol, k)]
type RemoveElem key ts = RemoveAt (FindElem key ts) ts

removeVecIndex :: Int -> VB.Vector a -> VB.Vector a
removeVecIndex a v = case VB.uncons back of
  Nothing -> front
  Just (_, back') -> front <> back'
  where
    (front, back) = VB.splitAt a v

remove :: forall key ts f. KnownNat (FindElem key ts) => Key key -> Rec f ts -> Rec f (Eval (RemoveAt (FindElem key ts) ts))
remove _ (Rec v) = Rec $ removeVecIndex (findElem @key @ts) v

-- test :: Rec Maybe '[ '("test", Int)]
-- test = empty & (#test) .~ (Just (5 :: Int))