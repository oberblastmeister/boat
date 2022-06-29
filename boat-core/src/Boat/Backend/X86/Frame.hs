{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Boat.Backend.X86.Frame
  ( FrameState,
    Frame,
    defFrameState,
    runFrame,
    interpretFrame,
    getStackSize,
  )
where

import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Boat.Backend.X86.X86 (pattern MemBaseSimple)
import Boat.Backend.X86.X86 qualified as X86
import Boat.Backend.Frame qualified as Frame

data FrameState = FrameState
  { base :: !Int
  }

makeFieldLabelsNoPrefix ''FrameState

type Frame = Frame.Frame X86.Mem

defFrameState :: FrameState
defFrameState = FrameState {base = -8}

runFrame :: Eff (Frame : es) a -> Eff es (a, FrameState)
runFrame = reinterpret (runState defFrameState) $ const interpretFrame

interpretFrame :: State FrameState :> es => Frame (Eff localEs) a -> Eff es a
interpretFrame (Frame.AllocLocalWith i) = do
  base <- use @FrameState #base
  modifying @FrameState #base (subtract $ fromIntegral i)
  pure $ MemBaseSimple $ fromIntegral base

getStackSize :: FrameState -> Int
getStackSize s = abs $ s ^. #base + 8
