module Oat.Common
  ( (++>),
    pattern (:>),
    ascii,
  )
where

import ASCII (ASCII)
import qualified ASCII

infixl 4 :>

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x : xs where (:>) xs x = x : xs

{-# COMPLETE (:>), [] #-}

(++>) :: [a] -> [a] -> [a]
(++>) l l' = l' ++ l

ascii :: ASCII.StringSuperset s => s -> ASCII s
ascii s = case ASCII.validateString s of
  Nothing -> error "Oat.Common.ascii: The string was not ascii!"
  Just s -> s