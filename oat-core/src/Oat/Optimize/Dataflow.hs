{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Optimize.Dataflow where

import Data.Sequence qualified as Seq
import Oat.Optimize.Block (Block, MaybeC (..), MaybeO (..), Shape (..))
import Oat.Optimize.Block qualified as Block
import Oat.Optimize.DGraph (DBlock (DBlock), DGraph)
import Oat.Optimize.Graph (Body, Graph, NonLocal (..))
import Oat.Optimize.Graph qualified as Graph
import Oat.Optimize.LabelMap (Label, LabelMap)
import Oat.Optimize.LabelMap qualified as LabelMap
import Oat.Utils.Optics (unwrap)

data ChangeFlag = NoChange | SomeChange
  deriving (Show, Eq, Ord)

type Join f = Label -> f -> f -> (ChangeFlag, f)

data ForwardTransfer n f
  = ForwardTransfer
      (n C O -> f -> f)
      (n O O -> f -> f)
      (n O C -> f -> FactBase f)

data ForwardRewrite m n f
  = ForwardRewrite
      (n C O -> f -> m (Maybe (Graph n C O, ForwardRewrite m n f)))
      (n O O -> f -> m (Maybe (Graph n O O, ForwardRewrite m n f)))
      (n O C -> f -> m (Maybe (Graph n O C, ForwardRewrite m n f)))

data ForwardPass m n f = ForwardPass
  { lattice :: Lattice f,
    transfer :: ForwardTransfer n f,
    rewrite :: ForwardRewrite m n f
  }

data BackwardTransfer n f
  = BackwardTransfer
      (n C O -> f -> f)
      (n O O -> f -> f)
      (n O C -> FactBase f -> f)

data BackwardRewrite m n f
  = BackwardRewrite
      (n C O -> f -> m (Maybe (Graph n C O, BackwardRewrite m n f)))
      (n O O -> f -> m (Maybe (Graph n O O, BackwardRewrite m n f)))
      (n O C -> f -> m (Maybe (Graph n O C, BackwardRewrite m n f)))

data BackwardPass m n f = BackwardPass
  { lattice :: Lattice f,
    transfer :: ForwardTransfer n f,
    rewrite :: ForwardRewrite m n f
  }

data Direction = Forward | Backward

-- type family Transfer d n f where
--   Transfer 'Forward n f = ForwardTransfer n f
--   Transfer 'Backward n f = BackwardTransfer n f

type Fact :: Shape -> Type -> Type
type family Fact x f where
  Fact C f = FactBase f
  Fact O f = f

data Lattice f = Lattice
  { name :: Text,
    bot :: f,
    join :: Join f
  }

changeIf :: Bool -> ChangeFlag
changeIf changed = if changed then SomeChange else NoChange

-- fixpoint ::
--   forall m n f.
--   (NonLocal n, Monad m) =>
--   Direction ->
--   Lattice f ->
--   Transfer f n ->
--   (Block n C C -> FactBase f -> m (Graph n C C, FactBase f)) ->
--   [Label] ->
--   Graph.Body n ->
--   FactBase f ->
--   m (Graph n C C, FactBase f)
-- fixpoint direction lattice transfer doBlock entries body factBase = undefined
--   where
--     blockDeps =
--       LabelMap.fromListWith
--         (++)
--         [ (l, [entryLabel b])
--           | b <- snd <$> toList body,
--             l <- case direction of
--               Forward -> [entryLabel b]
--               Backward -> successorLabels b
--         ]
--     loop ::
--       FactBase f ->
--       Seq Label ->
--       Graph.Body n ->
--       m (FactBase f, Graph.Body n)
--     loop factBase Seq.Empty newBody = pure (factBase, newBody)
--     loop factBase (lab Seq.:<| labTodo) newBody = do
--       let block = newBody ^. at lab % unwrap (error "cannot find block")
--       -- (resultGraph, )
--       pure undefined

-- forwardsFixpoint :: (NonLocal n) => Lattice f -> [Label] -> ForwardTransfer ->

-- blockFlowForwards :: Lattice

fixpoint ::
  forall m n fact.
  (NonLocal n, Monad m) =>
  Direction ->
  Lattice fact ->
  (Block n C C -> Fact C fact -> m (DGraph fact n C C, Fact C fact)) ->
  [Label] ->
  Body n ->
  Fact C fact ->
  m (DGraph fact n C C, Fact C fact)
fixpoint direction lattice doBlock entries body factBase = do
  (factBase', newBlocks) <- loop factBase (fromList entries) (mempty @(LabelMap _))
  pure
    ( Graph.Many NothingO newBlocks NothingO,
      foldl' (\m k -> m & at k .~ Nothing) factBase' (LabelMap.keys body)
    )
  where
    -- if the direction if forward, we essentially do @fmap entryLabel body@
    blockDeps =
      LabelMap.fromListWith
        (++)
        [ (l, [entryLabel b])
          | b <- snd <$> toList body,
            l <- case direction of
              Forward -> [entryLabel b]
              Backward -> successorLabels b
        ]
    loop ::
      FactBase fact ->
      Seq Label ->
      LabelMap (DBlock fact n C C) ->
      m (FactBase fact, LabelMap (DBlock fact n C C))
    loop factBase Seq.Empty newBlocks = pure (factBase, newBlocks)
    loop factBase (lab Seq.:<| labTodo) newBlocks = do
      case body ^. at lab of
        Nothing -> loop factBase labTodo newBlocks
        Just block -> do
          (resultDGraph, outFacts) <- doBlock block factBase
          let (changed, factBase') =
                LabelMap.foldrWithKey'
                  (updateFact lattice newBlocks)
                  ([], factBase)
                  outFacts
          let toAnalyze = concatMap (\l -> blockDeps ^. at l % non []) changed
          let newBlocks' = case resultDGraph of
                Graph.Many _ blocks _ -> LabelMap.union blocks newBlocks
          loop factBase' (labTodo <> fromList toAnalyze) newBlocks'

updateFact ::
  Lattice fact ->
  LabelMap (DBlock fact n C C) ->
  Label ->
  fact ->
  ([Label], FactBase fact) ->
  ([Label], FactBase fact)
updateFact lattice newBlocks lab newFact (changed, factBase)
  | NoChange <- changeFlag, has (ix lab) newBlocks = (changed, factBase)
  | otherwise = (lab : changed, factBase & at lab ?~ resultFact)
  where
    (changeFlag, resultFact) = case factBase ^. at lab of
      Nothing -> (SomeChange, newFactDebug)
      Just oldFact -> join oldFact
    join oldFact = lattice.join lab oldFact newFact
    (_, newFactDebug) = join lattice.bot

type FactBase fact = LabelMap fact

-- Lifting based on shape:
--  - from nodes to blocks
--  - from facts to fact-like things
-- Lowering back:
--  - from fact-like things to facts
-- Note that the latter two functions depend only on the entry shape.
class ShapeLifter e x where
  singletonDGraph :: f -> n e x -> DGraph f n e x
  forwardEntryFact :: NonLocal n => n e x -> f -> Fact e f
  forwardEntryLabel :: NonLocal n => n e x -> MaybeC e [Label]
  forwardTransfer :: ForwardTransfer n f -> n e x -> f -> Fact x f
  forwardRewrite :: ForwardRewrite m n f -> n e x -> f -> m (Maybe (Graph n e x, ForwardRewrite m n f))
  backwardEntryFact :: NonLocal n => Lattice f -> n e x -> Fact e f -> f
  backwardTransfer :: BackwardTransfer n f -> n e x -> Fact x f -> f
  backwardRewrite :: BackwardRewrite m n f -> n e x -> f -> m (Maybe (Graph n e x, BackwardRewrite m n f))

instance ShapeLifter C O where
  singletonDGraph fact node = Graph.exit (DBlock fact (Block.CO node Block.Empty))
  forwardEntryFact node fact = LabelMap.singleton (entryLabel node) fact
  forwardEntryLabel node = JustC [entryLabel node]
  forwardTransfer (ForwardTransfer transfer _ _) = transfer
  forwardRewrite (ForwardRewrite rewrite _ _) = rewrite
  backwardEntryFact lattice node factBase = getFact lattice (entryLabel node) factBase
  backwardTransfer (BackwardTransfer transfer _ _) = transfer
  backwardRewrite (BackwardRewrite rewrite _ _) = rewrite

instance ShapeLifter O O where
  singletonDGraph fact = Graph.single . DBlock fact . Block.Middle

getFact :: Lattice f -> Label -> FactBase f -> f
getFact lattice label factBase = factBase ^. at label % unwrap lattice.bot