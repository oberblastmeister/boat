module Oat.LL.Token.Kind where

data Kind
  = Let
  | Rec
  | Forall
  | True
  | False
  | In
  | If
  | Else
  | Then
  | Lambda
  | Num Int
  | Ident Text
  | ConIdent Text
  | Arrow
  | Assign
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | NEq
  | LParen
  | RParen
  | Dot
  | Error
  | EOF
  deriving (Show, Eq)