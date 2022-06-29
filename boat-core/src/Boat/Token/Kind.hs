module Boat.Token.Kind (Kind (..)) where

import Data.Int (Int64)

data Kind
  = Int !Int64
  | String !Text
  | Null
  | Ident !Text
  | UIdent !Text
  | TInt
  | TVoid
  | TString
  | TBool
  | If
  | Else
  | While
  | Return
  | Var
  | Struct
  | Semi
  | Comma
  | LBrace
  | RBrace
  | Plus
  | Dash
  | Start
  | EqEq
  | Eq
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Tilde
  | Bang
  | Global
  | For
  | True
  | False
  | Dot
  | New
  | Gt
  | Lt
  | LtEq
  | BangEq
  | Bar
  | Amper
  | IOr
  | IAnd
  | LtLt
  | GtGt
  | GtGtGt
  | Arrow
  | Eof
  deriving (Show, Eq)
