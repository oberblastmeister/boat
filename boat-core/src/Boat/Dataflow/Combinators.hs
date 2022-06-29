module Boat.Dataflow.Combinators where

import Boat.Dataflow.Run (ChangeFlag (..), FactPair (..), Lattice (..))

pairLattice :: forall f f'. Lattice f -> Lattice f' -> Lattice (f, f')
pairLattice l1 l2 =
  Lattice
    { name = l1.name <> " x " <> l2.name,
      bot = (l1.bot, l2.bot),
      join
    }
  where
    join lab FactPair {new = (new1, new2), old = (old1, old2)} = (c, (f1, f2))
      where
        (c1, f1) = l1.join lab FactPair {old = old1, new = new1}
        (c2, f2) = l2.join lab FactPair {old = old2, new = new2}
        c = case (c1, c2) of
          (NoChange, NoChange) -> NoChange
          _ -> SomeChange
