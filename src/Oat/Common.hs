module Oat.Common
  ( (++>),
    pattern (:>),
    ascii,
    internalError,
    unwrap,
    unreachable,
  )
where

import ASCII (ASCII)
import qualified ASCII
import Optics hiding ((:>))

infixl 4 :>

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x : xs where (:>) xs x = x : xs

{-# COMPLETE (:>), [] #-}

(++>) :: [a] -> [a] -> [a]
(++>) l l' = l' ++ l

ascii :: ASCII.StringSuperset s => s -> ASCII s
ascii s = case ASCII.validateString s of
  Nothing -> error "Oat.Common.ascii: The string was not ascii!"
  Just s -> s

internalError :: forall a. HasCallStack => Text -> a
internalError t = error $ "Internal compiler error: " <> t

-- non but only a getter
unwrap :: a -> Getter (Maybe a) a
unwrap def =
  to
    ( \case
        Just a -> a
        Nothing -> def
    )

unreachable :: forall a. a
unreachable = error "Unreachable!"

assert :: Bool -> Text -> ()
assert cond text =
  if cond
    then ()
    else internalError text