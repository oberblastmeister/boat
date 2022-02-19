module Oat.Backend.Frame
  ( Frame (..),
    allocLocalWith,
    allocLocal,
  )
where

data Frame :: Type -> Effect where
  AllocLocalWith :: Int -> Frame a m a

type instance DispatchOf (Frame a) = 'Dynamic

allocLocalWith :: forall a es. Frame a :> es => Int -> Eff es a
allocLocalWith i = send $ AllocLocalWith @a i

allocLocal :: forall a es. Frame a :> es => Eff es a
allocLocal = allocLocalWith @a 8
