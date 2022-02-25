{-# LANGUAGE QualifiedDo #-}

module Oat.Utils.OnLeft
  ( OnLeft (..),
    (>>=),
    return,
    toEither,
  )
where

import Prelude hiding (return, (>>=))

data OnLeft e a b = OnLeft (e -> b) (Either e a)

(>>=) :: OnLeft e t a -> (t -> Either a b) -> Either a b
(>>=) (OnLeft f (Left e)) _g = Left $ f e
(>>=) (OnLeft _f (Right a)) g = g a

toEither :: OnLeft e a b -> Either e a
toEither (OnLeft _ e) = e

return :: b -> Either a b
return = Right
