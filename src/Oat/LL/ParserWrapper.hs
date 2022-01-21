{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.ParserWrapper where

import Control.Monad.Except (MonadError)
import Control.Monad.Writer
import Oat.Common ((++>))
import Oat.LL.Lexer (alexMonadScan)
import Oat.LL.LexerWrapper (Alex (..), AlexState)
import Oat.LL.Token (Token (..))
import Optics
import Optics.State.Operators

data ParseError
  = LexerError Text

data ParseState = ParseState
  { -- errors in reverse order
    errors :: [ParseError],
    state :: AlexState
  }

newtype Parser a = Parser (ExceptT ParseError (StateT ParseState Identity) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState ParseState,
      MonadError ParseError
    )

makeFieldLabelsNoPrefix ''ParseState

-- instance Zoom Alex Parser AlexState ParseState where
--   zoom :: Is k A_Lens => Optic' k is ParseState AlexState -> Alex c -> Parser c
--   zoom o (Alex alex) = Parser $ lift $ zoom o alex

--   zoomMaybe :: Is k An_AffineTraversal => Optic' k is ParseState AlexState -> Alex c -> Parser (Maybe c)
--   zoomMaybe o (Alex alex) = Parser $ lift $ zoomMaybe o alex

--   zoomMany :: (Is k A_Traversal, Monoid c) => Optic' k is ParseState AlexState -> Alex c -> Parser c
--   zoomMany o (Alex alex) = Parser $ lift $ zoomMany o alex


-- -- implement the other MonadWriter stuff
-- instance MonadWriter [ParseError] Parser where
--   tell es = #errors %= (es ++>)

-- lexer :: (Token -> Parser a) -> Parser a
-- lexer k = zoom #alexState $ do
--   lexeme <- alexMonadScan
--   case lexeme of
--     Left e -> do
--       tell e
--       lexer k
--     Right to -> _
    
  
-- -- alexMonadScan <&> \case
-- --   Left e -> do tell [LexerError e]; lexer k
-- --   Right t -> k t

-- test :: Char
-- test = flip evalState ('a', 'b') $
--   zoom _1 $ do
--     act

-- -- test' :: forall m. MonadState Char m => m Char
-- -- test' :: forall m n. Zoom m n Char (Char, Char) => m Char
-- test' :: State (Char, Char) Char
-- test' = (zoom :: Optic' A_Lens NoIx (Char, Char) Char -> StateT Char Identity Char -> StateT (Char, Char) Identity Char) _1 $ do
--   act

-- test'' :: forall m n. Zoom m n Char (Char, Char) => n Char
-- test'' = (zoom :: Optic' A_Lens NoIx (Char, Char) Char -> m Char -> n Char) _1 $ do
--   act

-- act :: MonadState Char m => m Char
-- act = do
--   use equality