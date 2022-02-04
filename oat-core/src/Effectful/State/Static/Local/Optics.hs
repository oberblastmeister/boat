module Effectful.State.Static.Local.Optics
  ( use,
    preuse,
    modifying,
    assign,
    (.=),
    (?=),
    (%=),
  )
where

import Effectful (Eff, type (:>))
import Effectful.State.Static.Local

use :: (State s :> es, Is k A_Getter) => Optic' k is s a -> Eff es a
use o = gets (view o)
{-# INLINE use #-}

preuse :: (State s :> es, Is k An_AffineFold) => Optic' k is s a -> Eff es (Maybe a)
preuse o = gets (preview o)
{-# INLINE preuse #-}

modifying :: (State t :> es, Is k A_Setter) => Optic k is t t a b -> (a -> b) -> Eff es ()
modifying o = modify . over o
{-# INLINE modifying #-}

assign :: (State t :> es, Is k A_Setter) => Optic k is t t b a -> a -> Eff es ()
assign o = modifying o . const
{-# INLINE assign #-}

infix 4 .=, ?=, %=

(.=) :: (State t :> es, Is k A_Setter) => Optic k is t t b a -> a -> Eff es ()
(.=) = assign
{-# INLINE (.=) #-}

(?=) :: (State t :> es, Is k A_Setter) => Optic k is t t b (Maybe a) -> a -> Eff es ()
(?=) = \o -> assign o . Just
{-# INLINE (?=) #-}

(%=) :: (State t :> es, Is k A_Setter) => Optic k is t t a b -> (a -> b) -> Eff es ()
(%=) = modifying
{-# INLINE (%=) #-}