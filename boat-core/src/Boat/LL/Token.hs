module Boat.LL.Token (Token (..)) where

import Boat.LL.Token.Kind (Kind)

data Token = Token {kind :: Kind}
  deriving (Show, Eq)
