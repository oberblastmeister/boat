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
    unsafeASCIIKind,
    alexEOF,
  )
where

import Data.ASCII (ASCII)
import Data.ASCII qualified as ASCII
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

unsafeASCIIKind :: (ASCII ByteString -> Kind) -> AlexAction Lexeme
unsafeASCIIKind f = do
  text <- gview #text
  pure $ Right $ Token $ f $ ASCII.unsafeToASCII $ Text.Encoding.encodeUtf8 text

alexEOF :: Token
alexEOF = Token TK.Eof