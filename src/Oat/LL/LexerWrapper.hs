{-# LANGUAGE TemplateHaskell #-}

-- you need to define alexEOF and also import this wrapper
module Oat.LL.LexerWrapper where

import Oat.Wrappers.Lexer
import Text.Builder (Builder)

data User = User
  { textBuilder :: Builder
  }

defUser :: User
defUser = User {textBuilder = mempty}

makeLexer ''User 'defUser