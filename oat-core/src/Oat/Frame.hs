{-# LANGUAGE AllowAmbiguousTypes #-}

module Oat.Frame
  ( Frame (..),
    HasFrame (..),
    allocLocal,
    allocLocalM,
    framePointerM,
    returnRegM,
  )
where

import Oat.Asm.AST qualified as Asm

class Frame a where
  newFrame :: Int -> a
  allocLocalWith :: Int -> a -> (Asm.Mem a, a)
  allocGlobal :: ByteString -> a -> Asm.Mem a
  framePointer :: Asm.Reg a
  returnReg :: Asm.Reg a
  prologue :: Int -> Asm.Inst a

class HasFrame a b | a -> b where
  frame :: Optic' A_Lens NoIx a b

type UseFrame a b m = (Frame b, HasFrame a b, MonadState a m)

allocLocal :: (Frame a) => a -> (Asm.Mem a, a)
allocLocal = allocLocalWith 8

allocLocalWithM :: forall a b m. UseFrame a b m => Int -> m (Asm.Mem b)
allocLocalWithM i = do
  fr <- use frame
  let (mem, fr') = allocLocalWith i fr
  frame .= fr'
  pure mem

allocLocalM :: forall a b m. UseFrame a b m => m (Asm.Mem b)
allocLocalM = allocLocalWithM 8

framePointerM :: forall a b m. UseFrame a b m => m (Asm.Reg b)
framePointerM = pure $ framePointer @b

returnRegM :: forall a b m. UseFrame a b m => m (Asm.Reg b)
returnRegM = pure $ returnReg @b