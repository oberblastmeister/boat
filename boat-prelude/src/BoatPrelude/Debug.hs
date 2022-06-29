{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright:  (c) 2016 Stephen Diehl
--             (c) 2016-2018 Serokell
--             (c) 2018-2021 Kowainik
-- SPDX-License-Identifier: MIT
-- Maintainer:  Kowainik <xrom.xkov@gmail.com>
-- Stability:   Stable
-- Portability: Portable
--
-- This module contains functions for debugging __pure__ functions. You
-- can't use functions like 'System.IO.putStrLn' for this purpose because
-- they require changes to the type signature, but functions in this module
-- avoid this problem by being pure on their own.
--
-- Additionally, these functions produce compile-time warnings, if you leave them
-- in your code. Warnings help you to cleanup all debugging usages before
-- releasing.
--
-- @
-- __ghci>__ foo = trace "I forgot trace in code"
--
-- \<interactive\>:4:7: __warning__: [-Wdeprecations]
--     In the use of ‘trace’ (imported from "Relude"):
--     "'trace' remains in code"
-- @
--
-- The following table briefly shows names and types of all functions in
-- this module.
--
-- +-----------------+----------------------------------------+
-- | __Name__        | __Type__                               |
-- +=================+========================================+
-- | 'trace'         | @String -> a -> a@                     |
-- +-----------------+----------------------------------------+
-- | 'traceShow'     | @Show a => a -> b -> b@                |
-- +-----------------+----------------------------------------+
-- | 'traceShowId'   | @Show a => a -> a@                     |
-- +-----------------+----------------------------------------+
-- | 'traceShowWith' | @Show b => (a -> b) -> a -> a@         |
-- +-----------------+----------------------------------------+
-- | 'traceId'       | @String -> String@                     |
-- +-----------------+----------------------------------------+
-- | 'traceM'        | @(Applicative f) => String -> f ()@    |
-- +-----------------+----------------------------------------+
-- | 'traceShowM'    | @(Show a, Applicative f) => a -> f ()@ |
-- +-----------------+----------------------------------------+
--
-- __⚠ NOTE:__ Use these functions only for local debugging
-- purposes. They break referential transparency, they are only useful
-- when you want to observe intermediate values of your pure functions
-- and to understand the behaviour locally. If you want to log debug
-- messages in your application, consider using a logging library
-- instead.
module BoatPrelude.Debug
  ( -- * Tracing
    trace,
    traceShow,
    traceShowId,
    traceShowWith,
    traceId,
    traceM,
    traceShowM,

    -- * Imprecise error
    Undefined (..),
    undefined,
    dbg,
  )
where

import Data.Data (Data)
-- import Prelude.Applicative (Applicative)
-- import Relude.Base (Char, Constraint, Eq, Generic, HasCallStack, Ord, Show, Type, Typeable)
-- import Relude.Enum (Bounded, Enum)
-- import Relude.String (Read, String, Text, toString)

import Data.Typeable (Typeable)
import Debug.Trace qualified as Debug
import GHC.Exts (RuntimeRep, TYPE)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.IO.Unsafe qualified
import Text.Pretty.Simple (pPrint)
import Prelude (Applicative, Bool (True), Bounded, Enum, Eq, Ord, Read, Show, String, ($))
import Prelude qualified

-- $setup
-- >>> import Relude
-- >>> :set -Wno-deprecations

----------------------------------------------------------------------------
-- trace
----------------------------------------------------------------------------

debugFlag :: Bool
debugFlag = True

dbg :: Show a => a -> a
dbg a =
  if debugFlag
    then let !_ = System.IO.Unsafe.unsafePerformIO $ pPrint a in a
    else a
{-# WARNING dbg "'dbg' remains in code" #-}

-- | Prints the given 'String' message and returns the passed value of
-- type @a@.
--
-- >>> increment l = map (+1) l
-- >>> increment [2, 3, 4]
-- [3,4,5]
--
-- >>> increment l = trace ("incrementing each value of: " ++ show l) (map (+1) l)
-- >>> increment [2, 3, 4]
-- incrementing each value of: [2,3,4]
-- [3,4,5]
--
-- * If you want to print a 'Show'able value instead of 'String', use 'traceShow'.
-- * If you want to print the value itself, use 'traceShowId'.
-- * If you want to print by specifying a custom formatting function, use 'traceShowWith'.
trace :: String -> a -> a
trace = Debug.trace
{-# WARNING trace "'trace' remains in code" #-}

-- | Similar to 'trace' but prints a given value with the 'Show'
-- instance instead of a 'String'.
--
-- >>> increment l = map (+1) l
-- >>> increment [2, 3, 4]
-- [3,4,5]
--
-- >>> increment l = traceShow l (map (+1) l)
-- >>> increment [2, 3, 4]
-- [2,3,4]
-- [3,4,5]
--
-- * If you want to print a specific 'String' instead, use 'trace'
-- * If you want to print and return the same value, use 'traceShowId'
-- * If you want to specify a custom printing function, use 'traceShowWith'
traceShow :: Show a => a -> b -> b
traceShow = Debug.traceShow
{-# WARNING traceShow "'traceShow' remains in code" #-}

-- | Similar to 'traceShow' but prints the given value itself instead
-- of a separate value.
--
-- >>> traceShowId (1+2+3, "hello" ++ "world")
-- (6,"helloworld")
-- (6,"helloworld")
--
-- * If you to specify a different value to print, use 'trace' or 'traceShow'
-- * If you want to have more control over printing, use 'traceShowWith'
traceShowId :: Show a => a -> a
traceShowId = Debug.traceShowId
{-# WARNING traceShowId "'traceShowId' remains in code" #-}

-- | Similar 'traceShowId', but uses a provided function to convert the
-- argument to a value with the 'Show' constraint.
--
-- >>> traceShowWith fst (1, "ABC")
-- 1
-- (1,"ABC")
--
-- In other words, @'traceShowId' ≡ 'traceShowWith' id@.
--
-- This function is useful for debugging values that do not have 'Show'
-- instance:
--
-- >>> fst $ traceShowWith fst (1, id)
-- 1
-- 1
--
-- * If you don't need such flexibility, use simpler 'trace', 'traceShow' or 'traceShowId'
--
-- @since 1.0.0.0
traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f v = Debug.traceShow (f v) v
{-# WARNING traceShowWith "'traceShowWith remains in code" #-}

-- | Trace function to print values while working a pure monad
-- (e.g. 'Maybe', 'State', etc.)
--
-- >>> :{
-- let action :: Maybe Int
--    action = do
--        x <- Just 3
--        traceM ("x: " ++ show x)
--        y <- pure 12
--        traceM ("y: " ++ show y)
--        pure (x*2 + y)
-- in action
-- :}
-- x: 3
-- y: 12
-- Just 18
--
-- * If you want to print a value with the 'Show' instance instead, use 'traceShowM'
traceM :: (Applicative f) => String -> f ()
traceM = Debug.traceM
{-# WARNING traceM "'traceM' remains in code" #-}

-- |
-- Like 'traceM', but uses 'Relude.show' on the argument to convert it to a
-- 'String'.
--
-- >>> :{
-- let action :: Maybe Int
--     action = do
--         x <- Just 3
--         traceShowM x
--         y <- pure 12
--         traceShowM y
--         pure (x*2 + y)
-- in action
-- :}
-- 3
-- 12
-- Just 18
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Debug.traceShowM
{-# WARNING traceShowM "'traceShowM' remains in code" #-}

-- | Similar to 'traceShowId' but specialised for 'String'.
--
-- >>> traceId "hello"
-- "hello
-- hello"
traceId :: String -> String
traceId = Debug.traceId
{-# WARNING traceId "'traceId' remains in code" #-}

----------------------------------------------------------------------------
-- Undefined and undefined
----------------------------------------------------------------------------

-- | Similar to 'undefined' but data type.
data Undefined = Undefined
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable, Generic)
{-# WARNING Undefined "'Undefined' type remains in code" #-}

-- | 'Prelude.undefined' that leaves warning in code on every usage.
undefined :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => a
undefined = Prelude.undefined
{-# WARNING undefined "'undefined' function remains in code" #-}
