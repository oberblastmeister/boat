module Oat.LL.LowerGep where

import Oat.LL.AST qualified as LL

lowerGep :: LL.GepInst -> [LL.Inst]
lowerGep LL.GepInst {} = undefined