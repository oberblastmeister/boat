{
{-# LANGUAGE NoImplicitPrelude #-}

module Oat.LL.Parser
  (
    prog,
  )
where

import OldPrelude as Prelude
import Oat.LL.Token
import qualified Oat.LL.Token.Kind as Kind
import Oat.LL.ParserWrapper
import Oat.LL.AST
}

%name prog Prog

%tokentype { Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token {kind = Kind.Eof} }
%error { parseError }
%errorhandlertype explist

%token
  string { Token {kind = Kind.String $$ } }
  '*' { Token {kind = Kind.Star} }
  ',' { Token {kind = Kind.Comma} }
  '=' { Token {kind = Kind.Equals} }
  gid { Token {kind = Kind.Gid $$} }
  global { Token {kind = Kind.Global} }

%%

Prog :: { Prog }
Prog
  : error { undefined }

GDecl :: { GDecl }
GDecl
  : gid '=' global Ty { undefined }
  
GInit :: { GInit }
GInit
  : error { undefined }

Ty :: { Ty }
Ty
  : error { undefined }