{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Oat.Utils.Families
  ( type (++),
    Tail,
    Drop,
    Subset,
  )
where

import GHC.TypeLits (ErrorMessage (Text), Nat, TypeError, type (-))
import Prelude hiding (Map)

type Tail :: [a] -> [a]
type Tail xs = Drop 1 xs

type (++) :: [a] -> [a] -> [a]
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

infixr 5 ++

type Drop :: Nat -> [a] -> [a]
type family Drop n xs where
  Drop 0 xs = xs
  Drop n '[] = TypeError ('Text "Nothing left to drop!")
  Drop n (x ': xs) = Drop (n - 1) xs

type family Map f xs where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

class Subset es es'

instance Subset '[] es

instance (Subset es es', e :> es') => Subset (e : es) es'
