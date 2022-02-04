module Data.Infinite
  ( Infinite ((::>)),
    repeat,
    from,
    uncons,
    cons,
    head,
    tail,
  )
where

import Prelude hiding (cons, head, repeat, tail, uncons)
import Prelude qualified

newtype Infinite a = Infinite [a]
  deriving (Functor)

repeat :: a -> Infinite a
repeat = Infinite . Prelude.repeat

from :: Enum a => a -> Infinite a
from a = Infinite $ [a ..]

uncons :: Infinite a -> (a, Infinite a)
uncons (Infinite (a : as)) = (a, Infinite as)
uncons _ = error "impossible"

cons :: a -> Infinite a -> Infinite a
cons a (Infinite as) = Infinite $ a : as

head :: Infinite a -> a
head = fst . uncons

tail :: Infinite a -> Infinite a
tail = snd . uncons

pattern (::>) :: a -> Infinite a -> Infinite a
pattern a ::> as <-
  (uncons -> (a, as))
  where
    a ::> as = cons a as

{-# COMPLETE (::>) #-}

-- instance Cons (Infinite a) (Infinite a) a a where
--   _Cons = prism' (_) uncons