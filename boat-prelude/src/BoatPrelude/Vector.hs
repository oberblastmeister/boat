module BoatPrelude.Vector
  ( Vec,
    UVec,
    MVec,
    UMVec,
  )
where

import Data.Vector qualified as VB
import Data.Vector.Mutable qualified as VBM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

type Vec = VB.Vector

type UVec = VU.Vector

type MVec = VBM.MVector

type UMVec = VUM.MVector
