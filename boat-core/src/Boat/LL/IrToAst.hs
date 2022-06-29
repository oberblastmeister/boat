module Boat.LL.IrToAst
  ( funBodyToAst,
  )
where

import Data.Vector qualified as VB
import Effectful.Reader.Static (Reader, asks)
import Boat.Dataflow (Shape (..))
import Boat.Dataflow qualified as Dataflow
import Boat.Dataflow.Block qualified as Block
import Boat.Dataflow.Graph qualified as Dataflow.Graph
import Boat.Dataflow.Graph qualified as Graph
import Boat.Dataflow.TreeUtils qualified as Dataflow.TreeUtils
import Boat.LL.Ast qualified as Ast
import Boat.LL.Ir qualified as Ir
import Boat.LL.Name qualified as Ast
import Boat.Utils.Misc (accToVec)
import Boat.Utils.Optics (unwrap, (%??))
import Optics.Operators.Unsafe ((^?!))

type Effs =
  '[ Reader (Dataflow.LabelMap Ast.Name)
   ]

funBodyToAst :: Effs :>> es => Ir.FunBody -> Eff es Ast.FunBody
funBodyToAst body = case body.graph of
  Graph.Empty -> error "cannot have empty graph"
  Graph.Single {} -> error "cannot have single graph"
  graph@Graph.Many {entry = Dataflow.JustO entry, exit = Dataflow.JustO exit} -> do
    entry <- blockToAst entry
    let bodyBlocks = VB.fromList $ Dataflow.TreeUtils.preOrderF $ Dataflow.Graph.dfs Dataflow.NothingC graph
    labeled <- traverse blockToAst bodyBlocks
    exit <- blockToAst exit
    -- TODO: vector concatenation is propbably really slow
    pure Ast.FunBody {entry, labeled = labeled :> exit}

blockToAst ::
  Effs :>> es =>
  Ir.Block e x ->
  Eff es (Dataflow.IndexedCO2 e x Ast.LabBlock Ast.LabBlock Ast.Block Ast.Block)
blockToAst = \case
  Block.CC first (Block.OO insts) last -> do
    lab <- instToAst first
    insts <- traverse normalOpenInstToAst $ accToVec @Vec insts
    term <- instToAst last
    pure
      Ast.LabBlock
        { lab,
          block =
            Ast.Block
              { insts,
                term
              }
        }
  Block.CO first (Block.OO insts) -> do
    lab <- instToAst first
    insts <- traverse (instToAst @O @O) $ accToVec @Vec insts
    let (insts', term) = unsnocRet insts
    pure Ast.LabBlock {lab, block = Ast.Block {insts = insts', term}}
  Block.OC (Block.OO insts) last -> do
    insts <- traverse normalOpenInstToAst $ accToVec @Vec insts
    term <- instToAst last
    pure Ast.Block {insts, term}
  Block.OO insts -> do
    insts <- traverse instToAst $ accToVec @Vec insts
    let (insts', term) = unsnocRet insts
    pure Ast.Block {insts = insts', term}

normalOpenInstToAst :: Effs :>> es => Ir.Inst O O -> Eff es Ast.Inst
normalOpenInstToAst inst = do
  inst <- instToAst inst
  pure $ unwrapNormalOpen inst

unwrapNormalOpen :: Either Ast.Term Ast.Inst -> Ast.Inst
unwrapNormalOpen = (^. _Right %?? unwrap (error "cannot have a return in the middle of a block"))

unsnocRet :: HasCallStack => Vec (Either Ast.Term Ast.Inst) -> (Vec Ast.Inst, Ast.Term)
unsnocRet = \case
  insts :> (Left ret@(Ast.Ret _)) -> (fmap unwrapNormalOpen insts, ret)
  _ -> error "The last instruction of the exit block must have a ret terminator"

instToAst ::
  forall e x es.
  Effs :>> es =>
  Ir.Inst e x ->
  Eff es (Dataflow.IndexedCO2 e x Ast.Name Ast.Name Ast.Term (Either Ast.Term Ast.Inst))
instToAst = \case
  Ir.Label label -> getLabel label
  Ir.Inst inst -> pure $ Right inst
  Ir.Term term -> case term of
    Ir.Br label -> Ast.Br <$> getLabel label
    Ir.Cbr Ir.CbrTerm {ty, arg, lab1, lab2} -> do
      lab1 <- getLabel lab1
      lab2 <- getLabel lab2
      pure $ Ast.Cbr Ast.CbrTerm {ty, arg, lab1, lab2}
  Ir.Ret ret -> pure . Left . Ast.Ret $ ret

getLabel :: Effs :>> es => Dataflow.Label -> Eff es Ast.Name
getLabel label = asks @(Dataflow.LabelMap Ast.Name) (^?! ix label)
