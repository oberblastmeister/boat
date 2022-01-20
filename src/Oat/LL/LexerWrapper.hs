{-# LANGUAGE TemplateHaskell #-}

-- you need to define alexEOF and also import this wrapper
module Oat.LL.LexerWrapper where

import Data.Text.Lazy.Builder (Builder)
import Oat.Wrappers.Lexer

data User = User
  { textBuilder :: Builder
  }

defUser :: User
defUser = User {textBuilder = mempty}

makeLexer ''User 'defUser