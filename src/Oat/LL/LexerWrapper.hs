{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- you need to define alexEOF and also import this wrapper
module Oat.LL.LexerWrapper
  ( -- alex stuff
    AlexInput (..),
    AlexState (..),
    Alex (..),
    AlexEnv (..),
    AlexAction,
    alexMove,
    runAlex,
    alexGetByte,
    alexInputPrevChar,
    alexGetStartCode,
    alexSetStartCode,
    alexGetInput,
    alexSetInput,
    ignorePendingBytes,
    -- our stuff
    Lexeme,
    User (..),
    kind,
    stringKind,
    alexEOF,
  )
where

import Oat.LL.Token (Token (..))
import Oat.LL.Token.Kind (Kind)
import qualified Oat.LL.Token.Kind as TK
import Oat.Wrappers.Lexer (makeLexer)
import Optics
import qualified Text.Builder

data User = User
  { stringBuilder :: Text.Builder.Builder
  }

makeFieldLabelsNoPrefix ''User

defUser :: User
defUser = User {stringBuilder = mempty}

makeLexer ''User 'defUser

type Lexeme = Either Text Token

kind :: Kind -> AlexAction Lexeme
kind = pure . Right . Token

stringKind :: (Text -> Kind) -> AlexAction Lexeme
stringKind f = do
  text <- asks (^. #text)
  pure $ Right $ Token $ f text

alexEOF :: Token
alexEOF = Token TK.Eof