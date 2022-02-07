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
import Oat.LL.Lexer (alexMonadScan)
import Oat.LL.LexerWrapper (Alex (..), AlexState (..), defaultAlexState)
import Oat.LL.Token (Token (..))
import Optics.State.Operators

data ParseError
  = LexerError Text
  | UnexpectedToken {unexpected :: Token, expected :: [String]}
  deriving (Show, Eq)

data ParseState = ParseState
  { -- errors in reverse order
    errors :: [ParseError],
    alexState :: AlexState
  }

newtype Parser a = Parser {unParser :: ExceptT ParseError (StateT ParseState Identity) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState ParseState,
      MonadError ParseError
    )

makeFieldLabelsNoPrefix ''ParseState

parse :: Text -> Parser a -> Either [ParseError] a
parse text parser = fst $ runParser (defaultParseState text) parser

defaultParseState :: Text -> ParseState
defaultParseState text = ParseState {errors = [], alexState = defaultAlexState text}

runParser :: ParseState -> Parser a -> (Either [ParseError] a, ParseState)
runParser state parser = (res & _Left .~ (state'' ^. #errors), state'')
  where
    state'' = state' & #errors %~ (res ^.. _Left ++)
    (res, state') = parser & unParser & runExceptT & (`runState` state)

evalParser :: ParseState -> Parser a -> Either [ParseError] a
evalParser state = fst . runParser state

liftAlex :: Alex a -> Parser a
liftAlex alex = Parser $ zoom #alexState $ lift $ unAlex alex

tellErrors :: [ParseError] -> Parser ()
tellErrors es = #errors %= (es ++)

lexer :: (Token -> Parser b) -> Parser b
lexer k = do
  liftAlex alexMonadScan >>= \case
    Left e -> tellErrors [LexerError e] *> lexer k
    Right t -> k t

parseError :: forall a. (Token, [String]) -> Parser a
parseError (tok, exp) = throwError UnexpectedToken {unexpected = tok, expected = List.nub exp}