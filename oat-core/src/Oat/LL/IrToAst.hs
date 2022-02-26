module Oat.LL.IrToAst where

import Effectful.Reader.Static (Reader, asks)
import Effectful.Reader.Static.Optics (rview)
import Oat.Dataflow (Shape (..))
import Oat.Dataflow qualified as Dataflow
import Oat.Dataflow.Graph qualified as Graph
import Oat.LL.Ast qualified as Ast
import Oat.LL.Ast qualified as LL
import Oat.LL.Ir qualified as Ir
import Oat.LL.Name qualified as LL
import Optics.Operators.Unsafe ((^?!))

type Effs =
  '[ Reader (Dataflow.LabelMap LL.Name)
   ]

funBodyToAst :: Effs :>> es => Ir.FunBody -> Eff es LL.FunBody
funBodyToAst body = case body.graph of
  Graph.Empty -> error "cannot have empty graph"

-- Graph.Single block ->

-- blockToAst :: forall e x es. (Effs :>> es, Dataflow.GetShape e) => Ir.Block e x -> Eff es (Dataflow.IndexedCO e Ast.Block Ast.LabBlock)
-- blockToAst = case Dataflow.shape @e of 
--   Dataflow.O' -> 

-- instToAst :: Effs :>> es => Ir.Inst e O -> Eff es LL.Inst
-- instToAst (Ir.Label label) = asks @(Dataflow.LabelMap LL.Name) (^?! ix label)
-- instToAst (Ir.Inst inst) = pure inst

-- instToAst (Ir.Term term) = LL.Term <$> termToAst term

-- termToAst :: Effs :>> es => Ir.Term -> Eff es LL.Term
-- termToAst (Ir.Ret inst) = pure $ LL.Ret inst
-- termToAst (Ir.Br label) = LL.Br <$> getLabel label
-- termToAst (Ir.Cbr Ir.CbrTerm {ty, arg, lab1, lab2}) = do
--   lab1 <- getLabel lab1
--   lab2 <- getLabel lab2
--   pure $ LL.Cbr LL.CbrTerm {ty, arg, lab1, lab2}

getLabel :: Effs :>> es => Dataflow.Label -> Eff es LL.Name
getLabel label = asks @(Dataflow.LabelMap LL.Name) (^?! ix label)

-- instToAst (Ir.Inst inst) = inst
