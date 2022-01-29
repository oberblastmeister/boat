{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

-- you need to define alexEOF and also import this wrapper
module Oat.Wrappers.Lexer
  ( makeLexer,
  )
where

import qualified Data.ByteString as ByteString
import Data.Pos
import Data.Span
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import qualified Language.Haskell.TH as TH
import Optics
import qualified Relude.Unsafe as Unsafe

makeLexer :: TH.Name -> TH.Name -> TH.DecsQ
makeLexer name defUserState = do
  [d|
    data AlexInput = AlexInput
      { inpPos :: !Pos,
        inpPrev :: !Char,
        inpBytes :: !ByteString,
        inpText :: !Text
      }
      deriving (Show, Eq)

    data AlexState = AlexState
      { statePos :: !Pos,
        stateText :: !Text,
        stateCh :: !Char,
        stateBytes :: !ByteString,
        stateScd :: !Int, -- the current startcode
        user :: $con
      }

    newtype Alex a = Alex {unAlex :: (StateT AlexState Identity) a}
      deriving (Functor, Applicative, Monad, MonadState AlexState)

    data AlexEnv = AlexEnv
      { text :: !Text,
        span :: !Span
      }

    type AlexAction a = ReaderT AlexEnv Alex a

    instance LabelOptic "user" A_Lens AlexState AlexState $con $con where
      labelOptic = lens user (\s user -> s {user})

    instance LabelOptic "text" A_Lens AlexEnv AlexEnv Text Text where
      labelOptic = lensVL $ \f (AlexEnv text span) -> fmap (`AlexEnv` span) (f text)

    alexStartPos :: Pos
    alexStartPos = Pos 1 1

    alexMove :: Pos -> Char -> Pos
    alexMove (Pos line _) '\n' = Pos (line + 1) 1
    alexMove (Pos line col) _ = Pos line (col + 1)

    runAlex :: AlexState -> Alex a -> a
    runAlex alexState alex =
      alex
        & coerce
        & (`evalStateT` alexState)
        & runIdentity

    defaultAlexState :: Text -> AlexState
    defaultAlexState stateText =
      AlexState
        { stateBytes = Empty,
          statePos = alexStartPos,
          stateText,
          stateCh = '\n',
          stateScd = 0,
          user = $(TH.varE defUserState)
        }

    -- These two functions are needed for alex to work
    alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
    alexGetByte AlexInput {inpPos, inpPrev, inpBytes = b :< bs, inpText} =
      Just (b, AlexInput {inpPos, inpPrev, inpBytes = bs, inpText})
    alexGetByte AlexInput {inpText = Empty} = Nothing
    alexGetByte AlexInput {inpPos, inpText} =
      Just (b, AlexInput {inpPos = inpPos', inpPrev = c, inpBytes = bs, inpText = text'})
      where
        inpPos' = alexMove inpPos c
        (b, bs) = Unsafe.fromJust $ ByteString.uncons $ Text.Encoding.encodeUtf8 t
        (c, _) = Unsafe.fromJust $ T.uncons t
        (t, text') = T.splitAt 1 inpText

    alexInputPrevChar :: AlexInput -> Char
    alexInputPrevChar AlexInput {inpPrev} = inpPrev

    alexGetStartCode :: Alex Int
    alexGetStartCode = gets stateScd

    alexSetStartCode :: Int -> Alex ()
    alexSetStartCode stateScd = modify $ \st -> st {stateScd}

    alexGetInput :: Alex AlexInput
    alexGetInput = do
      AlexState {statePos, stateText, stateCh, stateBytes} <- get
      return $ AlexInput {inpPos = statePos, inpText = stateText, inpPrev = stateCh, inpBytes = stateBytes}

    alexSetInput :: AlexInput -> Alex ()
    alexSetInput AlexInput {inpPos, inpText, inpPrev, inpBytes} =
      modify $ \st -> (st {statePos = inpPos, stateText = inpText, stateCh = inpPrev, stateBytes = inpBytes})

    ignorePendingBytes :: AlexInput -> AlexInput
    ignorePendingBytes inp = inp {inpBytes = Empty}
    |]
  where
    con = TH.conT name