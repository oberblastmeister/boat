{-# LANGUAGE AllowAmbiguousTypes #-}

module Oat.Asm.Class
  ( Asm (..),
    AsmConstraint,
  )
where

class Asm a where
  type Reg a
  type Mem a
  type Imm a
  type OpCode a

type AsmConstraint :: (Type -> Constraint) -> Type -> Constraint

type AsmConstraint c a = (c (Reg a), c (Mem a), c (Imm a), c (OpCode a))
