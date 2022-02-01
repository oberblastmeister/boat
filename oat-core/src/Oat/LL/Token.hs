module Oat.LL.Token (Token (..)) where

import Oat.LL.Token.Kind (Kind)

data Token = Token {kind :: Kind}
  deriving (Show, Eq)