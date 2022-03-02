{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.ParserWrapper
  ( ParseError (..),
    ParseState (..),
    Parser (..),
    runParser,
    evalParser,
    liftAlex,
    tellErrors,
    lexer,
    parseError,
    parse,
    defaultParseState,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.List qualified as List
import Effectful.Error.Static qualified as Error.Static
import Oat.Error (CompileFail)
import Oat.LL.Lexer (alexMonadScan)
import Oat.LL.LexerWrapper (Alex (..), AlexState (..), defaultAlexState)
import Oat.LL.Token (Token (..))
import Oat.Reporter (Reporter)
import Oat.Reporter qualified as Reporter
import Oat.TH (makeFieldLabelsForOnly)
import Optics.State.Operators
import Prelude hiding (exp)

data ParseError
  = LexerError Text
  | UnexpectedToken {unexpected :: Token, expected :: [String]}
  deriving (Show, Eq)

data ParseState = ParseState
  { -- errors in reverse order
    errors :: [ParseError],
    alexState :: AlexState
  }

newtype Parser a = Parser {unParser :: ExceptT ParseError (State ParseState) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState ParseState,
      MonadError ParseError
    )
    via (ExceptT ParseError (State ParseState))

$(makeFieldLabelsForOnly ["errors"] ''ParseState)

-- sometimes hls complains about not finding interface file, presumably because AlexState was generated by th
instance LabelOptic "alexState" A_Lens ParseState ParseState AlexState AlexState where
  labelOptic = lens (.alexState) (\parseState alexState -> parseState {alexState})

parse :: '[Reporter [ParseError], Error.Static.Error CompileFail] :>> es => Text -> Parser a -> Eff es a
parse text parser = case fst $ runParser (defaultParseState text) parser of
  Left es -> Reporter.reportFail es
  Right a -> pure a

defaultParseState :: Text -> ParseState
defaultParseState text = ParseState {errors = [], alexState = defaultAlexState text}

runParser :: ParseState -> Parser a -> (Either [ParseError] a, ParseState)
runParser st parser = (res & _Left .~ (st'' ^. #errors), st'')
  where
    st'' = st' & #errors %~ (res ^.. _Left ++)
    (res, st') = parser.unParser & runExceptT & (`runState` st)

evalParser :: ParseState -> Parser a -> Either [ParseError] a
evalParser st = fst . runParser st

liftAlex :: Alex a -> Parser a
liftAlex alex = Parser $ zoom #alexState $ lift $ alex.unAlex

tellErrors :: [ParseError] -> Parser ()
tellErrors es = #errors %= (es ++)

lexer :: (Token -> Parser b) -> Parser b
lexer k = do
  liftAlex alexMonadScan >>= \case
    Left e -> tellErrors [LexerError e] *> lexer k
    Right t -> k t

parseError :: forall a. (Token, [String]) -> Parser a
parseError (tok, exp) = throwError UnexpectedToken {unexpected = tok, expected = List.nub exp}
