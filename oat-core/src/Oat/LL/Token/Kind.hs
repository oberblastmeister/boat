module Oat.LL.Token.Kind where

data Kind
  = Star
  | Comma
  | Colon
  | Equals
  | LParen
  | RParen
  | RBrace
  | LBrace
  | LBracket
  | RBracket
  | Cross
  | I1
  | I8
  | I32
  | I64
  | To
  | Br
  | Eq
  | Neq
  | Or
  | And
  | Add
  | Sub
  | Mul
  | Xor
  | Slt
  | Sle
  | Sgt
  | Sge
  | Shl
  | Ret
  | Type
  | Null
  | Lshr
  | Ashr
  | Call
  | Icmp
  | Void
  | Load
  | Store
  | Label
  | Entry
  | Global
  | Define
  | Declare
  | External
  | Alloca
  | Bitcast
  | Select
  | Sext
  | Gep
  | Int !Int
  | Lab !ByteString
  | Gid !ByteString
  | Uid !ByteString
  | String !ByteString
  | Eof
  deriving (Show, Eq)