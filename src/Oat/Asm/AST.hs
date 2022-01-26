-- the abstract assembly within a function body
{-# LANGUAGE UndecidableInstances #-}

module Oat.Asm.AST where

import qualified Oat.LL as LL
import Optics
import Optics.State.Operators

class Frame a where
  type Reg a :: Type
  type Mem a :: Type

  newFrame :: Int -> a
  allocLocalWith :: Int -> a -> (Mem a, a)
  allocGlobal :: LL.Name -> a -> Mem a

class HasFrame a b | a -> b where
  frameLens :: Optic' A_Lens NoIx a b

type AsmConstraint :: (Type -> Constraint) -> Type -> Constraint
type AsmConstraint c a = (c a, c (Reg a), c (Mem a))

data Loc a
  = LReg !(Reg a)
  | LMem !(Mem a)
  | LTemp !LL.Name

deriving instance AsmConstraint Show a => Show (Loc a)

deriving instance AsmConstraint Eq a => Eq (Loc a)

data Operand a
  = Const !Int64
  | Loc !(Loc a)

deriving instance AsmConstraint Show a => Show (Operand a)

deriving instance AsmConstraint Eq a => Eq (Operand a)

data Inst a
  = InsOp (OpIns a)
  | InsLabel !LL.Name
  | InsMove (MoveIns a)

deriving instance AsmConstraint Show a => Show (Inst a)

deriving instance AsmConstraint Eq a => Eq (Inst a)

data OpIns a = OpIns {op :: !a, src :: [Operand a], dst :: [Operand a]}

deriving instance AsmConstraint Show a => Show (OpIns a)

deriving instance AsmConstraint Eq a => Eq (OpIns a)

-- invariant, both must not be memory locations
data MoveIns a = MoveIns {src :: Operand a, dst :: Operand a}

deriving instance AsmConstraint Show a => Show (MoveIns a)

deriving instance AsmConstraint Eq a => Eq (MoveIns a)

allocLocal :: (Frame a) => a -> (Mem a, a)
allocLocal = allocLocalWith 8

allocLocalWithM :: forall a b m. (Frame b, HasFrame a b, MonadState a m) => Int -> m (Mem b)
allocLocalWithM i = do
  frame <- use frameLens
  let (mem, frame') = allocLocalWith i frame
  frameLens .= frame'
  pure mem

allocLocalM :: (Frame b, HasFrame a b, MonadState a m) => m (Mem b)
allocLocalM = allocLocalWithM 8