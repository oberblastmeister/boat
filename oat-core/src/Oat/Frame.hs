{-# LANGUAGE AllowAmbiguousTypes #-}

module Oat.Frame
  ( Frame (..),
    allocLocalWith,
    allocLocal,
  )
where

import Oat.Asm qualified as Asm

data Frame :: Type -> Effect where
  AllocLocalWith :: Int -> Frame a m (Asm.Mem a)

type instance DispatchOf (Frame a) = 'Dynamic

allocLocalWith :: forall a es. Frame a :> es => Int -> Eff es (Asm.Mem a)
allocLocalWith i = send $ AllocLocalWith @a i

allocLocal :: forall a es. Frame a :> es => Eff es (Asm.Mem a)
allocLocal = allocLocalWith @a 8
