module Control.RunMonad where

import Effectful.Error.Static

newtype RunMonad m a = RunMonad (m a)

data Either' e a b = Either' (e -> b) (Either e a)

(>>=) :: Either' e a b -> (a -> b) -> b
(>>=) (Either' f (Left e)) _g = f e
(>>=) (Either' _f (Right a)) g = g a

-- (>>=) forall a b. m a -> (a -> m b) -> m b
-- (>>=) :: Either e a -> (e -> Either e' a) -> (Either e' a)
-- (>>=)
runSomething :: Eff es ()
runSomething = do
  res <-
    runError @Int something
      & runError @Bool
      & runError @Char
  let final = case res of
        Left e -> Left $ show e
        Right rest -> case rest of
          Left e -> Left $ show e
          Right rest -> case rest of
            Left e -> Left $ show e
            Right a -> Right a

  pure ()

something :: '[Error Int, Error Bool, Error Char] :>> es => Eff es ()
something = pure ()

-- runMultiple :: '[Error Int, Error Bool, Error Char] :>> es => Eff