{-# LANGUAGE QuantifiedConstraints #-}

module Data.Some
  ( Some (..),
    Some1 (..),
    withSome,
    withSome1,
  )
where

data Some :: (a -> Type) -> Type where
  Some :: !(f a) -> Some f

deriving instance (forall a. (Show (f a))) => (Show (Some f))

withSome :: (forall a. f a -> b) -> Some f -> b
withSome f = \case
  Some x -> f x

data Some1 :: (a -> b -> Type) -> Type where
  Some1 :: !(f a b) -> Some1 f

deriving instance (forall a b. (Show (f a b))) => (Show (Some1 (f)))

withSome1 :: (forall a b. f a b -> c) -> Some1 f -> c
withSome1 f = \case
  Some1 x -> f x
