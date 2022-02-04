{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Oat.Frame
  ( IsFrame (..),
    HasFrame,
    MonadFrame,
    allocLocal,
    allocLocalM,
    framePointerM,
    returnRegM,
  )
where

import Oat.Asm.AST qualified as Asm

-- type HasAsm
class IsFrame a where
  newFrame :: Int -> a
  allocLocalWith :: Int -> a -> (Asm.Mem a, a)
  allocGlobal :: ByteString -> a -> Asm.Mem a
  framePointer :: Asm.Reg a
  returnReg :: Asm.Reg a
  prologue :: Int -> Asm.Inst a
  _Call :: Prism' (Asm.OpCode a) ()
  _Move :: Prism' (Asm.OpCode a) ()

-- needTemps :: Traversal (Asm.Inst a) (Asm.Inst a) (Asm.Mem a, Bool) LL.Name
-- makeMove :: (Asm.Operand a) -> (Asm.Operand a) -> [Asm.Inst a]

type HasFrame :: Type -> Type -> Constraint
type HasFrame s f = LabelOptic' "frame" A_Lens s f

type MonadFrame s m f = (MonadState s m, IsFrame f, HasFrame s f)

allocLocal :: (IsFrame a) => a -> (Asm.Mem a, a)
allocLocal = allocLocalWith 8

allocLocalWithM :: forall s m f. MonadFrame s m f => Int -> m (Asm.Mem f)
allocLocalWithM i = do
  fr :: a <- use #frame
  let (mem, fr') = allocLocalWith i fr
  #frame .= fr'
  pure mem

allocLocalM :: forall s m a. MonadFrame s m a => m (Asm.Mem a)
allocLocalM = allocLocalWithM 8

framePointerM :: forall s m f. MonadFrame s m f => m (Asm.Reg f)
framePointerM = pure $ framePointer @f

returnRegM :: forall s m f. MonadFrame s m f => m (Asm.Reg f)
returnRegM = pure $ returnReg @f