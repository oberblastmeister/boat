{
{-# LANGUAGE NoImplicitPrelude #-}

module Oat.LL.Lexer (alexMonadScan, tokenize) where

import OldPrelude as Prelude
import Data.Void (Void)
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Function ((&))
import Oat.LL.LexerWrapper
import qualified Oat.LL.Token.Kind as Kind
import Oat.LL.Token (Token(Token))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Read as Text.Read
import Data.Maybe (fromJust)
import GHC.Records
import Debug.Trace
import qualified Data.Span as Span
import Data.Span (Span(SpanP))
import Oat.LL.Token.Kind (Kind)
import Optics
import Optics.Operators
import Optics.Operators.Unsafe ((^?!))
import qualified Text.Builder
import qualified Data.Text.Encoding as Text.Encoding
import Optics.State.Operators
import Data.ByteString qualified as ByteString
}

$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]
$char = [$lower $upper]
$identStart = [$char $digit \_]
$identRest = [$identStart \.]
@ident = $identStart $identRest*
$newline = [\n]

tokens :-
  <0> $newline { skip }
  <0> $white+ { skip }
  <0> c \"  { start string } -- "
  <0> "*" { kind Kind.Star }
  <0> "," { kind Kind.Comma }
  <0> ":" { kind Kind.Colon }
  <0> "=" { kind Kind.Equals }
  <0> "(" { kind Kind.LParen }
  <0> ")" { kind Kind.RParen }
  <0> "{" { kind Kind.LBrace }
  <0> "}" { kind Kind.RBrace }
  <0> "[" { kind Kind.LBracket }
  <0> "]" { kind Kind.RBracket }
  <0> "i1" { kind Kind.I1 }
  <0> "i8" { kind Kind.I8 }
  <0> "i32" { kind Kind.I32 }
  <0> "i64" { kind Kind.I64 }
  <0> "to" { kind Kind.To }
  <0> "br" { kind Kind.Br }
  <0> "eq" { kind Kind.Eq }
  <0> "ne" { kind Kind.Neq }
  <0> "or" { kind Kind.Or }
  <0> "and" { kind Kind.And }
  <0> "add" { kind Kind.Add }
  <0> "sub" { kind Kind.Sub }
  <0> "mul" { kind Kind.Mul }
  <0> "xor" { kind Kind.Xor }
  <0> "slt" { kind Kind.Slt }
  <0> "sle" { kind Kind.Sle }
  <0> "sgt" { kind Kind.Sgt }
  <0> "sge" { kind Kind.Sge }
  <0> "shl" { kind Kind.Shl }
  <0> "ret" { kind Kind.Ret }
  <0> "getelementptr" { kind Kind.Gep }
  <0> "type" { kind Kind.Type }
  <0> "null" { kind Kind.Null }
  <0> "lshr" { kind Kind.Lshr }
  <0> "ashr" { kind Kind.Ashr }
  <0> "call" { kind Kind.Call }
  <0> "icmp" { kind Kind.Icmp }
  <0> "void" { kind Kind.Void }
  <0> "load" { kind Kind.Load }
  <0> "entry" { kind Kind.Entry }
  <0> "store" { kind Kind.Store }
  <0> "label" { kind Kind.Label }
  <0> "global" { kind Kind.Global }
  <0> "define" { kind Kind.Define }
  <0> "declare" { kind Kind.Declare }
  <0> "external" { kind Kind.External }
  <0> "alloca" { kind Kind.Alloca }
  <0> "bitcast" { kind Kind.Bitcast }
  <0> "select" { kind Kind.Select }
  <0> "zext" { kind Kind.Zext }
  <0> "sext" { kind Kind.Sext }
  <0> "%" "."? @ident { bytesKind $ Kind.Uid . ByteString.drop 1 }
  <0> "@" "."? @ident { bytesKind $ Kind.Gid . ByteString.drop 1 }
  <0> "x" { kind Kind.Cross }
  <0> "-"? $digit+ { stringKind $ \t -> Kind.Int $ Text.Read.decimal t ^?! _Right % _1 }
  <0> [a-z]+ { bytesKind Kind.Lab }
  <0> @ident { bytesKind Kind.Lab }
  <0> ";" [^ \n \r]* $newline { skip }
  <0> "declare" [^ \n \r]* $newline { skip }
  <string> \\ { do #user % #stringBuilder %= (<> Text.Builder.char '\\'); skip }
  <string> [^ \" \\]+ -- "
    { do
        text <- asks (^. #text)
        #user % #stringBuilder %= (<> Text.Builder.text text)
        skip
    }
  <string> \\ 00 \" { endString } -- "
  <string> . { do text <- asks (^. #text); pure $ Left $ "Unknown string character " <> text }
  <0> . { do text <- asks (^. #text); pure $ Left $ "Unkown token: " <> text }
    
{
  
-- make syntax highlighting work
type Dummy = Void

skip :: AlexAction Lexeme
skip = lift $ alexMonadScan

start :: Int -> AlexAction Lexeme
start sc = lift $ do
  alexSetStartCode sc
  alexMonadScan

endString :: AlexAction Lexeme
endString = do
  str <- #user % #stringBuilder <<.= ""
  lift $ alexSetStartCode 0
  pure $ Right $ Token $ Kind.String $ Text.Encoding.encodeUtf8 $ Text.Builder.run str

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
      let text = T.take len $ inp.inpText
          pos1 = inp.inpPos
          pos2 = inp'.inpPos 
          span = SpanP pos1 pos2
          env = AlexEnv text span
      runReaderT action env
      
tokenize :: Text -> [Lexeme]
tokenize text = runAlex text tokenize'

tokenize' ::  Alex [Lexeme]
tokenize' = do
  lex <- alexMonadScan
  case lex of
    Right (Token Kind.Eof) -> pure [lex]
    _ -> do
      lexes <- tokenize'
      pure $ lex : lexes

}
