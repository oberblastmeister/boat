module OatPrelude.Effectful
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
  ( StaticRep,
    evalStaticRep,
    execStaticRep,
    getStaticRep,
    localStaticRep,
    putStaticRep,
    runStaticRep,
    stateStaticRep,
    stateStaticRepM,
  )