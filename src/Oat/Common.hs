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

-- non but only a lens
unwrap :: a -> Lens' (Maybe a) a
unwrap def =
  lens
    (fromMaybe def)
    ( \m b -> case m of
        Just _ -> Just b
        Nothing -> Just def
    )

unreachable :: forall a. a
unreachable = error "Unreachable!"