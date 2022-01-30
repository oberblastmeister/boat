{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.ParserWrapper where

import Control.Monad.Identity
import Data.List qualified as List
import Oat.LL.Lexer (alexMonadScan)
import Oat.LL.LexerWrapper (Alex (..), AlexState (..))
import Oat.LL.Token (Token (..))

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

runParser :: Parser a -> ParseState -> (Either [ParseError] a, ParseState)
runParser parser state = (res & _Left .~ (state'' ^. #errors), state'')
  where
    state'' = state' & #errors %~ (res ^.. _Left ++)
    (res, state') = parser.unParser & runExceptT & usingState state

evalParser :: Parser a -> ParseState -> Either [ParseError] a
evalParser parser state = runParser parser state ^. _1

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