module Oat.LL.Tree where

import qualified Oat.LL.AST as LL
import Oat.LL.Name (Name)

data Operand
  = Null
  | Const {val :: !Int64}
  | Gid {name :: !Name}
  | Temp {name :: !Name, rest :: Tree}

data Tree
  = BinOp
      { op :: LL.BinOp,
        ty :: LL.Ty,
        arg1 :: Operand,
        arg2 :: Operand
      }
  | Alloca {ty :: LL.Ty}
  | Load {ty :: LL.Ty, arg :: Operand}
