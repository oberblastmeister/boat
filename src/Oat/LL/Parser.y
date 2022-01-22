{
{-# LANGUAGE NoImplicitPrelude #-}

module Oat.LL.Parser
  ( prog,
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
import Data.ASCII (ASCII)
import Data.ByteString (ByteString)
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
  ':' { Token {kind = Kind.Colon} }
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
  i32 { Token {kind = Kind.I32} }
  i64 { Token {kind = Kind.I64} }
  to { Token {kind = Kind.To} }
  br { Token {kind = Kind.Br} }
  eq { Token {kind = Kind.Eq} }
  neq { Token {kind = Kind.Neq} }
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
  : List(Decl) { Prog {decls = $1} }
  
Decl :: { Decl }
  : GlobalDecl { $1 }
  | TyDecl { $1 }
  | ExternDecl { $1 }
  | FunDecl { $1 }
  
FunDecl :: { Decl }
  : define Ty gid '(' ParamList ')' '{' EntryBlock List(LabBlock) '}'
    { DeclFun
        { name = $3,
          funDecl = 
            FunDecl
              { funTy = FunTy {args = map fst $5, ret = $2},
                params = map snd $5,
                cfg = FunBody {entry = $8, labeled = $9}
              }
        }
    }

ParamList :: { [(Ty, Name)] }
  : ListSep(Param, ',') { $1 }

Param :: { (Ty, Name) }
  : Ty uid { ($1, $2) }

ExternDecl :: { Decl }
  : declare Ty gid '(' TyList ')' { DeclExtern {name = $3, ty = TyFun FunTy {args = $5, ret = $2}}}
  | gid '=' external global Ty { DeclExtern {name = $1, ty = $5} }

GlobalDecl :: { Decl }
  : gid '=' global Ty GlobalInit
    { DeclGlobal
        { name = $1,
          globalDecl = GlobalDecl {ty = $4, globalInit = $5}
        }
    }
  
GlobalInit :: { GlobalInit }
  : null { GlobalNull }
  | gid { GlobalGid $1 }
  | int { GlobalInt $ fromIntegral $1 }
  | string { GlobalString $1 }
  | '[' GlobalDeclList ']' { GlobalArray $2 }
  | '{' GlobalDeclList '}' { GlobalStruct $2 }
  
GlobalDeclList :: { [GlobalDecl] }
  : ListSep(InsideArrayGlobalDecl, ',') { $1 }

InsideArrayGlobalDecl :: { GlobalDecl }
  : Ty GlobalInit { GlobalDecl {ty = $1, globalInit = $2} }

TyDecl :: { Decl }
  : uid '=' type Ty { DeclTy {name = $1, ty = $4} }

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
  
TyList :: { [Ty] }
  : ListSep(Ty, ',') { $1 }
  
EntryBlock :: { Block }
  : entry ':' Block { $3 }
  | Block { $1 }

LabBlock :: { LabBlock }
  : lab ':' Block { LabBlock {lab = $1, block = $3} }

Block :: { Block }
  : List(Ins) Terminator { Block {ins = $1, terminator = $2} }
  
Ins :: { Named Ins }
  : uid '=' BinOpIns { Named $1 $ BinOp $3 }
  | uid '=' AllocaIns { Named $1 $ Alloca $3 }
  | uid '=' LoadIns { Named $1 $ Load $3 }
  | StoreIns { Do $ Store $1 }
  | uid '=' IcmpIns { Named $1 $ Icmp $3 }
  | CallIns { Do $ Call $1 }
  | uid '=' CallIns { Named $1 $ Call $3 }
  | uid '=' BitcastIns { Named $1 $ Bitcast $3 }
  | uid '=' GepIns { Named $1 $ Gep $3 }

BinOpIns :: { BinOpIns }
  : BinOp Ty Operand ',' Operand  { BinOpIns {op = $1, ty = $2, arg1 = $3, arg2 = $5} }
  
AllocaIns :: { AllocaIns }
  : alloca Ty { AllocaIns {ty = $2} }
  
LoadIns :: { LoadIns }
  : load Ty ',' Ty Operand { LoadIns {ty = $4, arg = $5} }

StoreIns :: { StoreIns }
  : store Ty Operand ',' Ty Operand { StoreIns {ty = $2, arg1 = $3, arg2 = $6} }
  
IcmpIns :: { IcmpIns }
  : icmp CmpOp Ty Operand ',' Operand { IcmpIns {op = $2, ty = $3, arg1 = $4, arg2 = $6} }

CallIns :: { CallIns }
  : call Ty Operand '(' ArgList ')' { CallIns {ty = $2, fn = $3, args = $5} }
  
BitcastIns :: { BitcastIns }
  : bitcast Ty Operand to Ty { BitcastIns {from = $2, arg = $3, to = $5} }

GepIns :: { GepIns }
  : getelementptr Ty ',' Ty Operand ',' ListSep(GepOperand, ',') { GepIns {ty = $4, arg = $5, args = $7} }
  
GepOperand :: { Operand }
  : i64 Operand { $2 }
  | i32 Operand { $2 }

ArgList :: { [(Ty, Operand)] }
  : ListSep(Arg, ',') { $1 }
  
Arg :: { (Ty, Operand) }
  : Ty Operand { ($1, $2) }

CmpOp :: { CmpOp }
  : eq { Eq }
  | neq { Neq }
  | slt { Slt }
  | sle { Sle }
  | sgt { Sgt }
  | sge { Sge }

BinOp :: { BinOp }
  : add { Add }
  | sub { Sub }
  | mul { Mul }
  | shl { Shl }
  | lshr { Lshr }
  | ashr { Ashr }
  | and { And }
  | or { Or }
  | xor { Xor }

Terminator :: { Terminator }
  : ret Ty Maybe(Operand) { Ret RetTerm {ty = $2, arg = $3} }
  | br label uid { Br $3 }
  | br i1 Operand ',' label uid ',' label uid { Cbr CbrTerm {arg = $3, lab1 = $6, lab2 = $9} }

Operand :: { Operand }
  : null { Null }
  | int { Const $ fromIntegral $1 }
  | gid { Gid $1 }
  | uid { Temp $1 }
  
Maybe(p) :: { Maybe p }
  : {- empty -} { Nothing }
  | p { Just $1 }
  
List(p) :: { [p] }
  : {- empty -} { [] }
  | List1(p) { NE.toList $1 }

List1(p) :: { NonEmpty p }
  : ListRev1(p) { NE.reverse $1 }

ListRev1(p) :: { NonEmpty p }
  : p { pure $1 }
  | ListRev1(p) p { $2 `NE.cons` $1 }

ListSep(p, s) :: { [p] }
  : {- empty -} { [] }
  | ListSep1(p, s) { NE.toList $1 }

ListSep1(p, s) :: { NonEmpty p }
  : ListSepRev1(p, s) { NE.reverse $1 }

ListSepRev1(p, s) :: { NonEmpty p }
  : p { pure $1 }
  | ListSep1(p, s) s p { $3 `NE.cons` $1 }