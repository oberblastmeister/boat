-- you need to define alexEOF and also import this wrapper
module Oat.Wrappers.Lexer where

import Control.Lens (pattern (:<))
import qualified Control.Lens as L
import qualified Data.ByteString as ByteString
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import Oat.Span
import qualified Relude.Unsafe as Unsafe

data AlexInput = AlexInput
  { pos :: !Pos,
    prev :: !Char,
    bytes :: !ByteString,
    text :: !Text
  }
  deriving (Show, Eq)

data AlexState = AlexState
  { pos :: !Pos,
    text :: !Text,
    ch :: !Char,
    bytes :: !ByteString,
    scd :: !Int -- the current startcode
  }

newtype Alex a = Alex {unAlex :: (StateT AlexState Identity) a}
  deriving (Functor, Applicative, Monad, MonadState AlexState)

data AlexEnv = AlexEnv
  { tokText :: !Text,
    span :: !Span
  }

type AlexAction a = ReaderT AlexEnv Alex a

alexStartPos :: Pos
alexStartPos = Pos {line = 1, col = 1}

alexMove :: Pos -> Char -> Pos
alexMove Pos {line} '\n' = Pos {line = line + 1, col = 1}
alexMove Pos {line, col} _ = Pos {line, col = col + 1}

runAlex :: Text -> Alex a -> a
runAlex text alex =
  alex
    & unAlex
    & (`evalStateT` defaultAlexState text)
    & runIdentity

defaultAlexState :: Text -> AlexState
defaultAlexState text =
  AlexState
    { bytes = L.Empty,
      pos = alexStartPos,
      text,
      ch = '\n',
      scd = 0
    }

-- These two functions are needed for alex to work
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {pos, prev, bytes = b :< bs, text} =
  Just (b, AlexInput {pos, prev, bytes = bs, text})
alexGetByte AlexInput {text = L.Empty} = Nothing
alexGetByte AlexInput {pos, text} =
  Just (b, AlexInput {pos = pos', prev = c, bytes = bs, text = text'})
  where
    pos' = alexMove pos c
    (b, bs) = Unsafe.fromJust $ ByteString.uncons $ Text.Encoding.encodeUtf8 t
    (c, _) = Unsafe.fromJust $ T.uncons t
    (t, text') = T.splitAt 1 text

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar AlexInput {prev} = prev

alexGetStartCode :: Alex Int
alexGetStartCode = gets scd

alexSetStartCode :: Int -> Alex ()
alexSetStartCode scd = modify $ \st -> st {scd}

alexGetInput :: Alex AlexInput
alexGetInput = do
  AlexState {pos, text, ch, bytes} <- get
  return $ AlexInput {pos, text, prev = ch, bytes}

alexSetInput :: AlexInput -> Alex ()
alexSetInput AlexInput {pos, text, prev, bytes} =
  modify $ \st -> st {pos, text, ch = prev, bytes}

eofSpan :: Span
eofSpan = Span (Pos (-1) (-1)) (Pos (-1) (-1))

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes inp = inp {bytes = L.Empty}