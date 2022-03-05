{
{-# LANGUAGE NoImplicitPrelude #-}

module Oat.LL.Parser
  ( mod,
  )
where

import OldPrelude qualified as Prelude
import OldPrelude hiding (mod)
import Oat.LL.Token
import Oat.LL.Token.Kind qualified as Kind
import Oat.LL.ParserWrapper
import Oat.LL.Ast
import Oat.LL.Name
import Data.Sequence qualified as Seq
import Optics
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.ByteString (ByteString)
import Data.Vector qualified as VB
import OatPrelude.Vector
}

%name mod Module

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
  '[' { Token {kind = Kind.LBracket} }
  ']' { Token {kind = Kind.RBracket} }
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
  select { Token {kind = Kind.Select} }
  zext { Token {kind = Kind.Zext} }
  sext { Token {kind = Kind.Sext} }
  int { Token {kind = Kind.Int $$} }
  lab { Token {kind = Kind.Lab $$} }
  gid { Token {kind = Kind.Gid $$} }
  uid { Token {kind = Kind.Uid $$} }
  string { Token {kind = Kind.String $$ } }

%%

Module :: { Vec Decl }
  : Vec(Decl) { $1 }
  
Decl :: { Decl }
  : GlobalDecl { $1 }
  | TyDecl { $1 }
  | ExternDecl { $1 }
  | FunDecl { $1 }
  
FunDecl :: { Decl }
  : define Ty gid '(' ParamList ')' '{' EntryBlock Vec(LabBlock) '}'
    { DeclFun
        $3
        FunDecl
          { funTy = FunTy {args = VB.map fst $5, ret = $2},
            params = VB.map snd $5,
            body = FunBody {entry = $8, labeled = $9}
          }
    }

ParamList :: { Vec (Ty, Name) }
  : VecSep(Param, ',') { $1 }

Param :: { (Ty, Name) }
  : Ty uid { ($1, $2) }

ExternDecl :: { Decl }
  : declare Ty gid '(' TyList ')' { DeclExtern $3 (TyFun FunTy {args = $5, ret = $2}) }
  | gid '=' external global Ty { DeclExtern $1 $5 }

GlobalDecl :: { Decl }
  : gid '=' global Ty GlobalInit { DeclGlobal $1 GlobalDecl {ty = TyPtr $4, globalInit = $5} }
  
GlobalInit :: { GlobalInit }
  : null { GlobalNull }
  | gid { GlobalGid $1 }
  | int { GlobalInt $ fromIntegral $1 }
  | string { GlobalString $1 }
  | '[' GlobalDeclList ']' { GlobalArray $2 }
  | '{' GlobalDeclList '}' { GlobalStruct $2 }
  
GlobalDeclList :: { Vec GlobalDecl }
  : VecSep(InstideArrayGlobalDecl, ',') { $1 }

InstideArrayGlobalDecl :: { GlobalDecl }
  : Ty GlobalInit { GlobalDecl {ty = $1, globalInit = $2} }

TyDecl :: { Decl }
  : uid '=' type Ty { DeclTy $1 $4 }

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
  
TyList :: { Vec Ty }
  : VecSep(Ty, ',') { $1 }
  
EntryBlock :: { Block }
  : entry ':' Block { $3 }
  | Block { $1 }

LabBlock :: { LabBlock }
  : lab ':' Block { LabBlock {lab = $1, block = $3} }

Block :: { Block }
  : Vec(Inst) Term { Block {insts = $1, term = $2} }
  
Inst :: { Inst }
  : BinOpInst { BinOp $1 }
  | AllocaInst { Alloca $1 }
  | LoadInst { Load $1 } 
  | StoreInst { Store $1 }
  | IcmpInst { Icmp $1 }
  | CallInst { Call $1 }
  | BitcastInst { Bitcast $1 }
  | GepInst { Gep $1 }
  | SelectInst { Select $1 }
  | ExtInst { Ext $1 }

BinOpInst :: { BinOpInst }
  : uid '=' BinOp Ty Operand ',' Operand 
    {
      BinOpInst
        { name = $1,
          op = $3,
          ty = $4,
          arg1 = $5,
          arg2 = $7
        }
    }
  
AllocaInst :: { AllocaInst }
  : uid '=' alloca Ty { AllocaInst {name = $1, ty = $4} }
  
LoadInst :: { LoadInst }
  : uid '=' load Ty ',' Ty Operand { LoadInst {name = $1, ty = $4, ty' = $6, arg = $7} }

StoreInst :: { StoreInst }
  : store Ty Operand ',' Ty Operand
    {
      StoreInst
        { ty1 = $2,
          arg1 = $3,
          ty2 = $5,
          arg2 = $6
        }
    }
  
IcmpInst :: { IcmpInst }
  : uid '=' icmp CmpOp Ty Operand ',' Operand
    {
      IcmpInst
        { name = $1,
          op = $4,
          ty = $5,
          arg1 = $6,
          arg2 = $8
        }
    }

CallInst :: { CallInst }
  : Maybe(CallUid) call Ty Operand '(' ArgList ')'
    {
      CallInst
        { name = $1,
          ty = $3,
          fn = $4,
          args = $6
        }
    }
  
CallUid :: { Name }
  : uid '=' { $1 }
  
BitcastInst :: { BitcastInst }
  : uid '=' bitcast Ty Operand to Ty
    {
      BitcastInst
        { name = $1,
          ty1 = $4,
          arg = $5,
          ty2 = $7
        }
    }

GepInst :: { GepInst }
  : uid '=' getelementptr Ty ',' Ty Operand ',' VecSep1(GepOperand, ',')
    {
      GepInst
        { name = $1,
          ty' = $4,
          ty = $6,
          arg = $7,
          args = $9
        }
    }
  
GepOperand :: { (Ty, Operand) }
  : Ty Operand { ($1, $2) }
  -- this is the only place where we allow i32
  -- usually in LLVM i32 is used to index into structs
  -- we want to keep compatibility
  | i32 Operand { (I64, $2) }

SelectInst :: { SelectInst }
  : uid '=' select Ty Operand ',' Ty Operand ',' Ty Operand ','
    {
      SelectInst
        { name = $1,
          condTy = $4,
          cond = $5,
          ty1 = $7,
          arg1 = $8,
          ty2 = $10,
          arg2 = $11
        }
    }
    
ExtInst :: { ExtInst }
  : uid '=' ExtOp Ty Operand to Ty
    {
      ExtInst
        { name = $1,
          op = $3,
          ty1 = $4,
          arg = $5,
          ty2 = $7
        }
    }
  
ExtOp :: { ExtOp }
  : sext { Sext }
  | zext { Zext }

ArgList :: { Vec (Ty, Operand) }
  : VecSep(Arg, ',') { $1 }
  
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

Term :: { Term }
  : ret Ty Maybe(Operand) { Ret RetTerm {ty = $2, arg = $3} }
  | br label uid { Br $3 }
  | br Ty Operand ',' label uid ',' label uid
    {
      Cbr
        CbrTerm
          { ty = $2,
            arg = $3,
            lab1 = $6,
            lab2 = $9
          }
    }

Operand :: { Operand }
  : null { Const $ fromIntegral 0 }
  | int { Const $ fromIntegral $1 }
  | gid { Gid $1 }
  | uid { Temp $1 }
  
Maybe(p) :: { Maybe p }
  : {- empty -} { Nothing }
  | p { Just $1 }
  
Vec(p) :: { Vec p }
  : List(p) { VB.fromList $1 }
  
Vec1(p) :: { Vec p }
  : List1(p) { VB.fromList $ NE.toList $1 }
  
VecSep(p, s) :: { Vec p }
  : ListSep(p, s) { VB.fromList $1 }
  
VecSep1(p, s) :: { Vec p }
  : ListSep1(p, s) { VB.fromList $ NE.toList $1 }
  
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
