{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Oat.FrameAct
  ( -- IsFrameAct (..),
    FrameAct (..),
    allocLocalWith,
    allocLocal,
    allocGlobal,
    framePointer,
    returnReg,
    prologue,
    epilogue,
    -- HasFrameAct,
    -- MonadFrameAct,
    -- allocLocal,
    -- allocLocalM,
    -- framePointerM,
    -- returnRegM,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Oat.Asm.AST qualified as Asm

-- type HasAsm
-- class IsFrameAct a where
--   newFrameAct :: Int -> a
--   allocLocalWith :: Int -> a -> (Asm.Mem a, a)
--   allocGlobal :: ByteString -> a -> Asm.Mem a
--   framePointer :: Asm.Reg a
--   returnReg :: Asm.Reg a
--   prologue :: Int -> Asm.Inst a
--   _Call :: Prism' (Asm.OpCode a) ()
--   _Move :: Prism' (Asm.OpCode a) ()

data FrameAct :: Type -> Effect where
  AllocLocalWith :: Int -> FrameAct a m (Asm.Mem a)
  AllocGlobal :: ByteString -> FrameAct a m (Asm.Mem a)
  FrameActPointer :: FrameAct a m (Asm.Reg a)
  ReturnReg :: FrameAct a m (Asm.Reg a)
  Prologue :: Int -> FrameAct a m (Asm.Inst a)
  Epilogue :: Int -> FrameAct a m (Asm.Inst a)

type instance DispatchOf (FrameAct a) = 'Dynamic

allocLocalWith :: FrameAct a :> es => Int -> Eff es (Asm.Mem a)
allocLocalWith = send . AllocLocalWith

allocLocal :: FrameAct a :> es => Eff es (Asm.Mem a)
allocLocal = allocLocalWith 8

allocGlobal :: FrameAct a :> es => ByteString -> Eff es (Asm.Mem a)
allocGlobal = send . AllocGlobal

framePointer :: FrameAct a :> es => Eff es (Asm.Reg a)
framePointer = send FrameActPointer

returnReg :: FrameAct a :> es => Eff es (Asm.Reg a)
returnReg = send ReturnReg

prologue :: FrameAct a :> es => Int -> Eff es (Asm.Inst a)
prologue = send . Prologue

epilogue :: FrameAct a :> es => Int -> Eff es (Asm.Inst a)
epilogue = send . Epilogue

-- makeMove :: (Asm.Operand a) -> (Asm.Operand a) -> [Asm.Inst a]

-- type HasFrameAct :: Type -> Type -> Constraint
-- type HasFrameAct s f = LabelOptic' "frame" A_Lens s f

-- type MonadFrameAct s m f = (MonadState s m, IsFrameAct f, HasFrameAct s f)

-- allocLocal :: (IsFrameAct a) => a -> (Asm.Mem a, a)
-- allocLocal = allocLocalWith 8

-- allocLocalWithM :: forall s m f. MonadFrameAct s m f => Int -> m (Asm.Mem f)
-- allocLocalWithM i = do
--   fr :: a <- use #frame
--   let (mem, fr') = allocLocalWith i fr
--   #frame .= fr'
--   pure mem

-- allocLocalM :: forall s m a. MonadFrameAct s m a => m (Asm.Mem a)
-- allocLocalM = allocLocalWithM 8

-- framePointerM :: forall s m f. MonadFrameAct s m f => m (Asm.Reg f)
-- framePointerM = pure $ framePointer @f

-- returnRegM :: forall s m f. MonadFrameAct s m f => m (Asm.Reg f)
-- returnRegM = pure $ returnReg @f