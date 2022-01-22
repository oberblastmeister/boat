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
import qualified Data.Sequence as Seq
import Optics
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
}

%name prog Prog

%tokentype { Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token {kind = Kind.Eof} }
%error { parseError }
%errorhandlertype explist

%token
  '*' { Token {kind = Kind.Star} }
  ',' { Token {kind = Kind.Comma} }
  ';' { Token {kind = Kind.Colon} }
  '=' { Token {kind = Kind.Equals} }
  '(' { Token {kind = Kind.LParen} }
  ')' { Token {kind = Kind.RParen} }
  '{' { Token {kind = Kind.LBrace} }
  '}' { Token {kind = Kind.RBrace} }
  '[' { Token {kind = Kind.LParen} }
  ']' { Token {kind = Kind.RParen} }
  x { Token {kind = Kind.Cross} }
  i1 { Token {kind = Kind.I1} }
  i8 { Token {kind = Kind.I8} }
  i64 { Token {kind = Kind.I64} }
  to { Token {kind = Kind.To} }
  br { Token {kind = Kind.Br} }
  eq { Token {kind = Kind.Eq} }
  neq { Token {kind = Kind.NEq} }
  or { Token {kind = Kind.Or} }
  and { Token {kind = Kind.And} }
  add { Token {kind = Kind.Add} }
  sub { Token {kind = Kind.Sub} }
  mul { Token {kind = Kind.Mul} }
  xor { Token {kind = Kind.Xor} }
  slt { Token {kind = Kind.Slt} }
  sle { Token {kind = Kind.Sle} }
  sgt { Token {kind = Kind.Sgt} }
  sge { Token {kind = Kind.Sge} }
  shl { Token {kind = Kind.Shl} }
  ret { Token {kind = Kind.Ret} }
  type { Token {kind = Kind.Type} }
  null { Token {kind = Kind.Null} }
  lshr { Token {kind = Kind.Lshr} }
  ashr { Token {kind = Kind.Ashr} }
  call { Token {kind = Kind.Call} }
  icmp { Token {kind = Kind.Icmp} }
  void { Token {kind = Kind.Void} }
  load { Token {kind = Kind.Load} }
  store { Token {kind = Kind.Store} }
  label { Token {kind = Kind.Label} }
  entry { Token {kind = Kind.Entry} }
  global { Token {kind = Kind.Global} }
  define { Token {kind = Kind.Define} }
  declare { Token {kind = Kind.Declare} }
  external { Token {kind = Kind.External} }
  alloca { Token {kind = Kind.Alloca} }
  bitcast { Token {kind = Kind.Bitcast} }
  getelementptr { Token {kind = Kind.Gep} }
  int { Token {kind = Kind.Int $$} }
  lab { Token {kind = Kind.Lab $$} }
  gid { Token {kind = Kind.Gid $$} }
  uid { Token {kind = Kind.Uid $$} }
  string { Token {kind = Kind.String $$ } }

%%

Prog :: { Prog }
  : error { undefined }

GDecl :: { GDecl }
  : gid '=' global Ty { undefined }
  
GInit :: { GInit }
  : error { undefined }

Ty :: { Ty }
  : Ty '*' { TyPtr $1 }
  | NonPtrTy { $1 }
  
NonPtrTy :: { Ty }
  : void { Void }
  | i1 { I1 } 
  | i8 { I8 }
  | i64 { I64 }
  | '{' TyList '}' { TyStruct $2 }
  | '[' int x Ty ']' { TyArray $2 $4 }
  | Ty '(' TyList ')' { TyFun FunTy {args = $3, ret = $1}}
  | uid { TyNamed $1 }
  
TyList :: { NonEmpty Ty }
  : ManySep1(Ty, ',') { $1 }
  
ManySep1(p, s) :: { NonEmpty p }
  : ManySepRev1(p, s) { NE.reverse $1 }

ManySepRev1(p, s) :: { NonEmpty p }
  : p { pure $1 }
  | ManySep1(p, s) s p { $3 `NE.cons` $1 }