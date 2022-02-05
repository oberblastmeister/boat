module Oat.Frame
  ( Frame (..),
    allocLocalWith,
    allocLocal,
    allocGlobal,
    prologue,
    epilogue,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Oat.Asm qualified as Asm

data Frame :: Type -> Effect where
  AllocLocalWith :: Int -> Frame a m (Asm.Mem a)
  AllocGlobal :: ByteString -> Frame a m (Asm.Mem a)
  Prologue :: Int -> Frame a m (Asm.Inst a)
  Epilogue :: Int -> Frame a m (Asm.Inst a)

type instance DispatchOf (Frame a) = 'Dynamic

allocLocalWith :: Frame a :> es => Int -> Eff es (Asm.Mem a)
allocLocalWith = send . AllocLocalWith

allocLocal :: Frame a :> es => Eff es (Asm.Mem a)
allocLocal = allocLocalWith 8

allocGlobal :: Frame a :> es => ByteString -> Eff es (Asm.Mem a)
allocGlobal = send . AllocGlobal

prologue :: Frame a :> es => Int -> Eff es (Asm.Inst a)
prologue = send . Prologue

epilogue :: Frame a :> es => Int -> Eff es (Asm.Inst a)
epilogue = send . Epilogue