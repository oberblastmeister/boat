module Oat.Dataflow where

data Direction = Forward | Backward

type Fact :: Type -> Direction -> Type
data Fact a d = Fact {combine :: [a] -> a}