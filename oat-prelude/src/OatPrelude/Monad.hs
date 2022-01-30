module OatPrelude.Monad
  ( module X,
    usingState,
    usingStateT,
  )
where

import Control.Monad.Except as X
import Control.Monad.Identity as X
import Control.Monad.Reader as X
import Control.Monad.State.Strict as X
import Prelude (flip)

usingState :: s -> State s a -> (a, s)
usingState = flip runState

usingStateT :: s -> StateT s m a -> m (a, s)
usingStateT = flip runStateT