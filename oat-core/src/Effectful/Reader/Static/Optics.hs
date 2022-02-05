module Effectful.Reader.Static.Optics
  ( rview,
  )
where

import Effectful
import Effectful.Reader.Static

rview :: (Reader s :> es, Is k A_Getter) => Optic' k is s a -> Eff es a
rview o = asks (view o)