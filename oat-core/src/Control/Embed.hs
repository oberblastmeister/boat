module Control.Embed where

import Effectful.Dispatch.Dynamic (localSeqUnlift)

newtype Embed :: (Type -> Type) -> Effect where
  Embed :: {unEmbed :: m a} -> Embed m z a

type instance DispatchOf (Embed _) = 'Dynamic

embed :: Embed m :> es => m a -> Eff es a
embed = send . Embed

runEmbedded :: forall m1 m2 es a. Embed m2 :> es => (forall x. m1 x -> m2 x) -> Eff (Embed m1 ': es) a -> Eff es a
-- runEmbedded f = interpret $ const $ embed . f . unEmbed
runEmbedded f = interpret $ \_ -> \case
  Embed m -> embed $ f m
  
runEmbeddedIO :: IOE :> es => Eff (Embed IO ': es) a -> Eff es a
runEmbeddedIO = interpret $ \_ -> \case
  Embed m -> liftIO m