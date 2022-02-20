module Oat.Utils.Monad
  ( unlessM,
    whenM,
    whenJust,
    whenNothing,
    whenNothingM,
    whenJustM,
    whenOf,
    whenOfM,
    unlessOf,
    unlessOfM,
    whenOfM_,
    whenOf_,
    liftEither,
  )
where

import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Prelude hiding (maybe)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM m = condM >>= (`unless` m)

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM m = condM >>= (`when` m)

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust maybe f = case maybe of
  Nothing -> pure ()
  Just a -> f a

whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing maybe m = case maybe of
  Nothing -> m
  Just _ -> pure ()

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM maybeM f = maybeM >>= (`whenJust` f)

whenNothingM :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM maybeM m = maybeM >>= (`whenNothing` m)

whenOf :: (Is k An_AffineFold, Applicative m) => Optic' k is s a -> s -> (a -> m ()) -> m ()
whenOf o s fam = whenJust (s ^? o) fam

whenOf_ :: (Is k An_AffineFold, Applicative m) => Optic' k is p b -> p -> m () -> m ()
whenOf_ o s m = whenOf o s (const m)

whenOfM :: (Is k An_AffineFold, Monad m) => Optic' k is s a -> m s -> (a -> m ()) -> m ()
whenOfM o ms fam = ms >>= (\s -> whenOf o s fam)

whenOfM_ :: (Is k An_AffineFold, Monad m) => Optic' k is s b -> m s -> m () -> m ()
whenOfM_ o ms m = whenOfM o ms (const m)

unlessOf :: (Is k An_AffineFold, Applicative m) => Optic' k is s a -> s -> m () -> m ()
unlessOf o s m = whenNothing (s ^? o) m

unlessOfM :: (Is k An_AffineFold, Monad m) => Optic' k is s a -> m s -> m () -> m ()
unlessOfM o ms m = ms >>= (\s -> unlessOf o s m)

liftEither :: (Error e :> es) => Either e a -> Eff es a
liftEither (Left e) = Error.throwError e
liftEither (Right a) = pure a
