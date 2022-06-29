module Boat.Utils.CallStack
  ( module GHC.Stack,
    popnCallStack,
    withNBackCallStack,
    withCurrentCallStack,
    withCallerCallStack,
  )
where

import GHC.Stack

-- | Pops n entries off a @CallStack@ using @popCallStack@.
-- Note that frozen callstacks are unaffected.
popnCallStack :: Word -> CallStack -> CallStack
popnCallStack 0 = id
popnCallStack n = popnCallStack (n - 1) . popCallStack

withNBackCallStack :: HasCallStack => Word -> (CallStack -> b) -> b
withNBackCallStack n f = f (popnCallStack n from)
  where
    -- This very line (always dropped):
    here = callStack
    -- The invoker (n = 0):
    from = popCallStack here

withCurrentCallStack :: HasCallStack => (CallStack -> b) -> b
withCurrentCallStack = withNBackCallStack 0

-- 0 => this line in this utility function.
-- 1 => the invocation of this utility function.

withCallerCallStack :: HasCallStack => (CallStack -> b) -> b
withCallerCallStack = withNBackCallStack 1

-- 0 => this line in this utility function.
-- 1 => our caller.
-- 2 => their caller.
