{-# LANGUAGE TemplateHaskell #-}

module Boat.Backend.Frame
  ( Frame (..),
    allocLocalWith,
    allocLocal,
  )
where

data Frame :: Type -> Effect where
  AllocLocalWith :: Int -> Frame a m a

$(makeEffect ''Frame)

allocLocal :: forall a es. Frame a :> es => Eff es a
allocLocal = allocLocalWith @a 8
