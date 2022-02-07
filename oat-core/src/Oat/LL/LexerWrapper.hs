{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
    defaultAlexState,
    -- our stuff
    Lexeme,
    User (..),
    kind,
    stringKind,
    bytesKind,
    alexEOF,
  )
where

import Data.Text.Encoding qualified as Text.Encoding
import Oat.LL.Token (Token (Token))
import Oat.LL.Token.Kind (Kind)
import Oat.LL.Token.Kind qualified as TK
import Oat.Wrappers.Lexer (makeLexer)
import Text.Builder qualified

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
  text <- gview #text
  pure $ Right $ Token $ f text
  
bytesKind :: (ByteString -> Kind) -> AlexAction Lexeme
bytesKind f = do
  text <- gview #text
  pure $ Right $ Token $ f $ Text.Encoding.encodeUtf8 text

alexEOF :: Token
alexEOF = Token TK.Eof