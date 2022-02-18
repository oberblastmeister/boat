module Oat.Utils.Monad
  ( unlessM,
    whenM,
    whenJust,
    whenNothing,
    whenNothingM,
    whenJustM,
  )
where

import Prelude hiding (maybe)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM m = do
  cond <- condM
  unless cond m

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM m = do
  cond <- condM
  when cond m

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust maybe f = case maybe of
  Nothing -> pure ()
  Just a -> f a

whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing maybe m = case maybe of
  Nothing -> m
  Just _ -> pure ()

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM maybeM f =
  maybeM >>= \case
    Just a -> f a
    Nothing -> pure ()

whenNothingM :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM maybeM m =
  maybeM >>= \case
    Just _ -> pure ()
    Nothing -> m
