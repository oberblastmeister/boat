{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Boat.Utils.Families
  ( type (++),
    Tail,
    Drop,
    Constraints,
    Map,
    Exp,
    Eval,
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

type Constraints :: [Constraint] -> Constraint
type family Constraints xs where
  Constraints (x ': xs) = (x, Constraints xs)
  Constraints '[] = ()

type Exp a = a -> Type

-- | Expression evaluator.
type family Eval (e :: Exp a) :: a

-- ** Monadic operations

infixr 1 =<<, <=<

infixl 4 <$>, <*>

data Pure :: a -> Exp a

type instance Eval (Pure x) = x

data Pure1 :: (a -> b) -> a -> Exp b

type instance Eval (Pure1 f x) = f x

data Pure2 :: (a -> b -> c) -> a -> b -> Exp c

type instance Eval (Pure2 f x y) = f x y

data Pure3 :: (a -> b -> c -> d) -> a -> b -> c -> Exp d

type instance Eval (Pure3 f x y z) = f x y z

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b

type instance Eval (k =<< e) = Eval (k (Eval e))

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c

type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))

data Join :: Exp (Exp a) -> Exp a

type instance Eval (Join e) = Eval (Eval e)

data (<$>) :: (a -> b) -> Exp a -> Exp b

type instance Eval (f <$> e) = f (Eval e)

data (<*>) :: Exp (a -> b) -> Exp a -> Exp b

type instance Eval (f <*> e) = Eval f (Eval e)

data Flip :: (a -> b -> Exp c) -> b -> a -> Exp c

type instance Eval (Flip f y x) = Eval (f x y)
