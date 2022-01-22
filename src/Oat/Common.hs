module Oat.Common
  ( (++>),
    pattern (:>),
    internalError,
    unwrap,
    unreachable,
  )
where

import Optics hiding ((:>))

infixl 4 :>

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x : xs where xs :> x = x : xs

{-# COMPLETE (:>), [] #-}

(++>) :: [a] -> [a] -> [a]
(++>) l l' = l' ++ l

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