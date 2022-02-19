-- need these record extensions to not get errors about invalid variables because of record selectors
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

-- you need to define alexEOF and also import this wrapper
module Oat.Wrappers.Lexer
  ( makeLexer,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.ByteString qualified as ByteString
import Data.Pos (Pos (Pos))
import Data.Span (Span)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word8)
import Language.Haskell.TH qualified as TH
import OatPrelude.Unsafe qualified as Unsafe

makeLexer :: TH.Name -> TH.Name -> TH.DecsQ
makeLexer name defUserState = do
  [d|
    data AlexInput = AlexInput
      { inpPos :: !Pos,
        inpPrev :: !Char,
        inpBytes :: !ByteString,
        inpText :: !Text
      }
      deriving (Show)

    data AlexState = AlexState
      { statePos :: !Pos,
        stateText :: !Text,
        stateCh :: !Char,
        stateBytes :: !ByteString,
        stateScd :: !Int, -- the current startcode
        user :: $con
      }
      deriving (Show)

    newtype Alex a = Alex {unAlex :: (StateT AlexState Identity) a}
      deriving (Functor, Applicative, Monad, MonadState AlexState)

    data AlexEnv = AlexEnv
      { text :: !Text,
        span :: !Span
      }
      deriving (Show)

    type AlexAction a = ReaderT AlexEnv Alex a

    instance LabelOptic "user" A_Lens AlexState AlexState $con $con where
      labelOptic = lens user (\s user -> s {user})

    instance LabelOptic "text" A_Lens AlexEnv AlexEnv Text Text where
      labelOptic = lensVL $ \f (AlexEnv text span) -> fmap (`AlexEnv` span) (f text)

    alexMove :: Pos -> Char -> Pos
    alexMove (Pos line _ pos) '\n' = Pos (line + 1) 1 (pos + 1)
    alexMove (Pos line col pos) _ = Pos line (col + 1) (pos + 1)

    runAlex :: Text -> Alex a -> a
    runAlex text alex =
      alex
        & coerce
        & (`evalStateT` defaultAlexState text)
        & runIdentity

    alexDefaultPos :: Pos
    alexDefaultPos = Pos 1 1 1

    defaultAlexState :: Text -> AlexState
    defaultAlexState stateText =
      AlexState
        { stateBytes = Empty,
          statePos = alexDefaultPos,
          stateText,
          stateCh = '\n',
          stateScd = 0,
          user = $(TH.varE defUserState)
        }

    alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
    alexGetByte AlexInput {inpPos, inpPrev, inpBytes = b :< bs, inpText} =
      Just (b, AlexInput {inpPos, inpPrev, inpBytes = bs, inpText})
    alexGetByte AlexInput {inpText = Empty, inpBytes = Empty} = Nothing
    alexGetByte AlexInput {inpPos, inpText, inpBytes = Empty} =
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
