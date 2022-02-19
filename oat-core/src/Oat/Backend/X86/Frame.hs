{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.Frame
  ( FrameState,
    Frame,
    defFrameState,
    runFrame,
    interpretFrame,
    getStackSize,
    allocLocalWith,
    allocLocal,
  )
where

import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Oat.Backend.X86.X86 (pattern MemBaseSimple)
import Oat.Backend.X86.X86 qualified as X86

data FrameState = FrameState
  { base :: !Int
  }

makeFieldLabelsNoPrefix ''FrameState

data Frame :: Effect where
  AllocLocalWith :: Int -> Frame m X86.Mem

type instance DispatchOf Frame = 'Dynamic

allocLocalWith :: forall es. Frame :> es => Int -> Eff es X86.Mem
allocLocalWith i = send $ AllocLocalWith i

allocLocal :: forall es. Frame :> es => Eff es X86.Mem
allocLocal = allocLocalWith 8

defFrameState :: FrameState
defFrameState = FrameState {base = -8}

runFrame :: Eff (Frame : es) a -> Eff es (a, FrameState)
runFrame = reinterpret (runState defFrameState) $ const interpretFrame

interpretFrame :: State FrameState :> es => Frame (Eff localEs) a -> Eff es a
interpretFrame (AllocLocalWith i) = do
  base <- use @FrameState #base
  modifying @FrameState #base (subtract $ fromIntegral i)
  pure $ MemBaseSimple $ fromIntegral base

getStackSize :: FrameState -> Int
getStackSize s = abs $ s ^. #base + 8
