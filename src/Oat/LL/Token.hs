module Oat.LL.Token where

import Oat.LL.Token.Kind (Kind)

data Token = Token Kind
  deriving (Show, Eq)