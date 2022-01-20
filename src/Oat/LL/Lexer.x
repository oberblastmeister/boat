{
{-# LANGUAGE NoImplicitPrelude #-}

module Oat.LL.Lexer where

import OldPrelude
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Function ((&))
import Oat.LL.LexerWrapper
import qualified Oat.LL.Token.Kind as TK
import Oat.LL.Token (Token(..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromJust)
import GHC.Records
import Debug.Trace
import qualified Oat.Span as Span
import Oat.LL.Token.Kind (Kind)
import Optics
}

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$ident = [$digit $upper $lower '_' '\'']
$eol = [\n]
-- we put this separate because alex doesn't do string escapes like haskell
-- inside literal strings which is confusing but it does in regex
$lam = [\\]

tokens :-
  <0> $eol { skip }
  <0> $white+ { skip }
  -- <0> "c\"" { start string }
  -- <0> "--".* { skip }

  -- <0> "forall" { tk TK.Forall }
  -- <0> "let" { tk TK.Let }
  -- <0> "rec" { tk TK.Rec }
  -- <0> "in" { tk TK.In }
  -- <0> "if" { tk TK.If }
  -- <0> "then" { tk TK.Then }
  -- <0> "else" { tk TK.Else }
  -- <0> "True" { tk TK.True }
  -- <0> "False" { tk TK.False }
  -- <0> $digit+ { stringTok $ TK.Num . read . T.unpack }
  -- <0> "->" { tk TK.Arrow }
  -- <0> "=" { tk TK.Assign }
  -- <0> "==" { tk TK.Eq }
  -- <0> "!=" { tk TK.NEq }
  -- <0> $lam { tk TK.Lambda }
  -- <0> "+" { tk TK.Add }
  -- <0> "-" { tk TK.Sub }
  -- <0> "*" { tk TK.Mul }
  -- <0> "/" { tk TK.Div }
  -- <0> "(" { tk TK.LParen }
  -- <0> ")" { tk TK.RParen }
  -- <0> "." { tk TK.Dot }

  -- <0> $lower $ident* { stringTok $ TK.Ident }
  -- <0> $upper $ident* { stringTok $ TK.ConIdent }
  
  <0> .
    { do
        tokText <- asks tokText
        pure $ Left $ "Unkown token: " <> tokText
    }
    
{
  
type Lexeme = Either Text Token

tk :: Kind -> AlexAction Lexeme
tk = pure . Right . Token

stringTok :: (Text -> Kind) -> AlexAction Lexeme
stringTok f = do
  tokText <- asks tokText
  pure $ Right $ Token $ f tokText

alexEOF :: Token
alexEOF = Token TK.Eof

dbg :: Show a => a -> a
dbg x = trace ("DBG: " ++ show x) x

skip :: AlexAction Lexeme
skip = lift $ alexMonadScan

start :: Int -> AlexAction Lexeme
start sc = lift $ do
  alexSetStartCode sc
  alexMonadScan

alexMonadScan :: Alex Lexeme
alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> pure $ Right alexEOF
    AlexError inp ->
      error $ "BUG: should not happen, use the regex dot to find errors" ++ show inp
    AlexSkip inp' _len -> do
      alexSetInput inp'
      alexMonadScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      let tokText = T.take len $ inpText inp
          pos1 = inpPos inp
          pos2 = inpPos inp'
          span = Span.mkSpan pos1 pos2
          env = AlexEnv { tokText, span }
      runReaderT action env
}