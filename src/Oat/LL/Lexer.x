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

$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]
$char = [$lower $upper]
$identStart = [$char $digit '_']
$identRest = [$identStart '.']
@ident = $identStart $identRest*
$newline = [\n]

tokens :-
  <0> $newline { skip }
  <0> $white+ { skip }
  -- <0> "c\"" { start string }
  <0> "*" { tk TK.Star }
  <0> "," { tk TK.Comma }
  <0> ":" { tk TK.Colon }
  <0> "=" { tk TK.Equals }
  <0> "(" { tk TK.LParen }
  <0> ")" { tk TK.RParen }
  <0> "{" { tk TK.LBrace }
  <0> "}" { tk TK.RBrace }
  <0> "[" { tk TK.LBracket }
  <0> "]" { tk TK.RBracket }
  <0> "i1" { tk TK.I1 }
  <0> "i8" { tk TK.I8 }
  <0> "i32" { tk TK.I32 }
  <0> "i64" { tk TK.I64 }
  <0> "to" { tk TK.To }
  <0> "br" { tk TK.Br }
  <0> "eq" { tk TK.Eq }
  <0> "ne" { tk TK.NEq }
  <0> "or" { tk TK.Or }
  <0> "and" { tk TK.And }
  <0> "add" { tk TK.Add }
  <0> "sub" { tk TK.Sub }
  <0> "mul" { tk TK.Mul }
  <0> "xor" { tk TK.Xor }
  <0> "slt" { tk TK.Slt }
  <0> "sle" { tk TK.Sle }
  <0> "sgt" { tk TK.Sgt }
  <0> "sge" { tk TK.Sge }
  <0> "shl" { tk TK.Shl }
  <0> "ret" { tk TK.Ret }
  <0> "getelementptr" { tk TK.Gep }
  <0> "type" { tk TK.Type }
  <0> "null" { tk TK.Null }
  <0> "lshr" { tk TK.Lshr }
  <0> "ashr" { tk TK.Ashr }
  <0> "call" { tk TK.Call }
  <0> "icmp" { tk TK.Icmp }
  <0> "void" { tk TK.Void }
  <0> "load" { tk TK.Load }
  <0> "entry" { tk TK.Entry }
  <0> "store" { tk TK.Store }
  <0> "label" { tk TK.Label }
  <0> "global" { tk TK.Global }
  <0> "define" { tk TK.Define }
  <0> "declare" { tk TK.Declare }
  <0> "external" { tk TK.External }
  <0> "alloca" { tk TK.Alloca }
  <0> "bitcast" { tk TK.Bitcast }
  <0> "%" "."? @ident { stringTK TK.Uid }
  <0> "@" "."? @ident { stringTK TK.Gid }
  <0> "x" { tk TK.Cross }
  -- TODO: fix this
  <0> "-"? $digit+ { stringTK $ TK.Int . undefined }
  <0> @ident { stringTK $ TK.Lab }
  <0> ";" [^ \n \r]* $newline { skip }
  <0> "declare" [^ \n \r]* $newline { skip }

  <0> .
    { do
        tokText <- asks tokText
        pure $ Left $ "Unkown token: " <> tokText
    }
    
{
  
type Lexeme = Either Text Token

tk :: Kind -> AlexAction Lexeme
tk = pure . Right . Token

stringTK :: (Text -> Kind) -> AlexAction Lexeme
stringTK f = do
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