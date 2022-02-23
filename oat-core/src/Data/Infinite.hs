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

import Data.Data (Data)
import Oat.Utils.Impossible qualified as Impossible
import Prelude hiding (cons, head, repeat, tail, uncons)
import Prelude qualified

newtype Infinite a = Infinite [a]
  deriving (Functor, Data, Typeable, Generic)

repeat :: a -> Infinite a
repeat = Infinite . Prelude.repeat

from :: Enum a => a -> Infinite a
from a = Infinite [a ..]

uncons :: Infinite a -> (a, Infinite a)
uncons (Infinite (a : as)) = (a, Infinite as)
uncons _ = Impossible.impossible

cons :: a -> Infinite a -> Infinite a
cons a (Infinite as) = Infinite $ a : as

head :: Lens' (Infinite a) a
head = lens (fst . uncons) (\inf a -> cons a $ snd $ uncons inf)

tail :: Getter (Infinite a) (Infinite a)
tail = to $ snd . uncons

pattern (::>) :: a -> Infinite a -> Infinite a
pattern a ::> as <-
  (uncons -> (a, as))
  where
    a ::> as = cons a as

{-# COMPLETE (::>) #-}
