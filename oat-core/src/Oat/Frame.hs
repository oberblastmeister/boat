{-# LANGUAGE AllowAmbiguousTypes #-}

module Oat.Frame where

import Optics.State.Operators

class Frame a where
  type Reg a :: Type
  type Mem a :: Type
  type Imm a :: Type
  type OpCode a :: Type

  newFrame :: Int -> a
  allocLocalWith :: Int -> a -> (Mem a, a)
  allocGlobal :: ByteString -> a -> Mem a
  framePointer :: Reg a
  returnReg :: Reg a

class HasFrame a b | a -> b where
  frameLens :: Optic' A_Lens NoIx a b

type UseFrame a b m = (Frame b, HasFrame a b, MonadState a m)

allocLocal :: (Frame a) => a -> (Mem a, a)
allocLocal = allocLocalWith 8

allocLocalWithM :: forall a b m. UseFrame a b m => Int -> m (Mem b)
allocLocalWithM i = do
  frame <- use frameLens
  let (mem, frame') = allocLocalWith i frame
  frameLens .= frame'
  pure mem

allocLocalM :: forall a b m. UseFrame a b m => m (Mem b)
allocLocalM = allocLocalWithM 8

framePointerM :: forall a b m. UseFrame a b m => m (Reg b)
framePointerM = pure $ framePointer @b

returnRegM :: forall a b m. UseFrame a b m => m (Reg b)
returnRegM = pure $ returnReg @b