module Oat.LL.Token where

import GHC.Records
import Oat.LL.Token.Kind (Kind)
import Oat.Span

data Token = Token {kind :: Kind}
  deriving (Show, Eq)