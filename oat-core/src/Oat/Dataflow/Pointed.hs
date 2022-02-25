{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Dataflow.Pointed
  ( Pointed (..),
    WithBot,
    WithTop,
    WithBoth,
    lattice,
    lattice',
    extendJoinDomain,
  )
where

import Oat.Dataflow.Label (Label)
import Oat.Dataflow.Run (ChangeFlag (..), FactPair (..), Join, Lattice (..))
import Oat.Dataflow.Shape (Shape (..))

-- | Adds top, bottom, or both to help form a lattice
data Pointed :: Shape -> Shape -> Type -> Type where
  Bot :: Pointed e C a
  Top :: Pointed C x a
  Elem :: !a -> Pointed e x a

deriving instance (Show a) => Show (Pointed e x a)

deriving instance (Eq a) => Eq (Pointed e x a)
-- ^ The type parameters 't' and 'b' are used to say whether top
-- and bottom elements have been added.  The analogy with 'Block'
-- is nearly exact:
--
--  * A 'Block' is closed at the entry if and only if it has a first node;
--    a 'Pointed' is closed at the top if and only if it has a top element.
--
--  * A 'Block' is closed at the exit if and only if it has a last node;
--    a 'Pointed' is closed at the bottom if and only if it has a bottom element.
--
-- We thus have four possible types, of which three are interesting:
--
--  [@Pointed C C a@] Type @a@ extended with both top and bottom elements.
--
--  [@Pointed C O a@] Type @a@ extended with a top element
--  only. (Presumably @a@ comes equipped with a bottom element of its own.)
--
--  [@Pointed O C a@] Type @a@ extended with a bottom element only.
--
--  [@Pointed O O a@] Isomorphic to @a@, and therefore not interesting.
--
-- The advantage of all this GADT-ishness is that the constructors
-- 'Bot', 'Top', and 'PElem' can all be used polymorphically.
--
-- A 'Pointed t b' type is an instance of 'Functor' and 'Show'.

type WithBot a = Pointed O C a
-- ^ Type 'a' with a bottom element adjoined

type WithTop a = Pointed C O a
-- ^ Type 'a' with a top element adjoined

type WithBoth a = Pointed C C a
-- ^ Type 'a' with top and bottom elements adjoined

lattice :: Text -> Join a -> Lattice (Pointed t C a)
lattice name join = lattice' name join'
  where
    join' l factPair = (change, Elem f)
      where
        (change, f) = join l factPair

lattice' ::
  forall f t.
  Text ->
  (Label -> FactPair f -> (ChangeFlag, Pointed t C f)) ->
  Lattice (Pointed t C f)
lattice' name join' = Lattice {name, bot = Bot, join}
  where
    -- careful: order of cases matters for ChangeFlag
    join :: Join (Pointed t C f)
    join l = \case
      FactPair {new = Bot, old} -> (NoChange, old)
      FactPair {old = Top} -> (NoChange, Top)
      FactPair {new, old = Bot} -> (SomeChange, new)
      FactPair {new = Top} -> (SomeChange, Top)
      FactPair {new = Elem new, old = Elem old} -> join' l FactPair {new, old}

extendJoinDomain ::
  forall f.
  (Label -> FactPair f -> (ChangeFlag, WithTop f)) ->
  Join (WithTop f)
extendJoinDomain join' = join
  where
    join :: Join (WithTop f)
    join l = \case
      FactPair {old = Top} -> (NoChange, Top)
      FactPair {new = Top} -> (SomeChange, Top)
      FactPair {new = Elem new, old = Elem old} -> join' l FactPair {new, old}
