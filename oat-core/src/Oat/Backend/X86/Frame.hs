{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend.X86.Frame
  ( FrameState,
    Frame,
    defFrameState,
    runFrame,
    interpretFrame,
  )
where

import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Oat.Backend.X86.X86 (Mem (MemStackSimple), X86)
import Oat.Frame qualified as Frame

data FrameState = FrameState
  { stack :: !Int
  }

makeFieldLabelsNoPrefix ''FrameState

type Frame = Frame.Frame X86

defFrameState :: FrameState
defFrameState = FrameState {stack = 0}

runFrame :: Eff (Frame : es) a -> Eff es (a, FrameState)
runFrame = reinterpret (runState defFrameState) $ const interpretFrame

interpretFrame :: State FrameState :> es => Frame (Eff localEs) a -> Eff es a
interpretFrame (Frame.AllocLocalWith i) = do
  stack <- use #stack
  #stack %= (+ fromIntegral i)
  pure $ MemStackSimple $ fromIntegral stack
interpretFrame (Frame.AllocGlobal _) = undefined