module Oat.LL.Token where

import Oat.LL.Token.Kind (Kind)

data Token = Token {kind :: Kind}
  deriving (Show, Eq)