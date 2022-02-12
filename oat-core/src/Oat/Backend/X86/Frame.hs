{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.Frame
  ( FrameState,
    Frame,
    defFrameState,
    runFrame,
    interpretFrame,
  getStackSize)
where

import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Oat.Backend.X86.X86 (X86, pattern MemBaseSimple)
import Oat.Frame qualified as Frame

data FrameState = FrameState
  { base :: !Int
  }

makeFieldLabelsNoPrefix ''FrameState

type Frame = Frame.Frame X86

defFrameState :: FrameState
defFrameState = FrameState {base = -8}

runFrame :: Eff (Frame : es) a -> Eff es (a, FrameState)
runFrame = reinterpret (runState defFrameState) $ const interpretFrame

interpretFrame :: State FrameState :> es => Frame (Eff localEs) a -> Eff es a
interpretFrame (Frame.AllocLocalWith i) = do
  base <- use #base
  #base %= subtract (fromIntegral i)
  pure $ MemBaseSimple $ fromIntegral base


getStackSize :: FrameState -> Int
getStackSize s = abs $ s ^. #base + 8
