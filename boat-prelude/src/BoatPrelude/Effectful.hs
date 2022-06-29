module BoatPrelude.Effectful
  ( module X,
  )
where

import Effectful as X
  ( Dispatch (Dynamic, Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    runPureEff,
    type (:>),
    type (:>>),
  )
import Effectful.Dispatch.Dynamic as X
  ( interpret,
    reinterpret,
    send,
  )
import Effectful.Dispatch.Static as X
  ( SideEffects (..),
    StaticRep,
    evalStaticRep,
    execStaticRep,
    getStaticRep,
    localStaticRep,
    putStaticRep,
    runStaticRep,
    stateStaticRep,
    stateStaticRepM,
  )
import Effectful.TH as X (makeEffect, makeEffect_)
