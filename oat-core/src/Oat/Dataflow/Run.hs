{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Dataflow.Run
  ( ChangeFlag (..),
    Join,
    ForwardTransfer,
    ForwardRewrite (..),
    ForwardPass (..),
    BackwardTransfer (..),
    BackwardRewrite (..),
    BackwardPass (..),
    Fact,
    Lattice (..),
    FactBase,
    FactPair (..),
    changeIf,
    runForward,
    runBackward,
    makeFactBase,
    backwardTransfer1,
    backwardRewrite3,
    noBackwardRewrite,
    backwardRewrite1,
    noRewrite,
  )
where

import Acc qualified
import Data.Sequence qualified as Seq
import Oat.Dataflow.Block (Block)
import Oat.Dataflow.Block qualified as Block
import Oat.Dataflow.FactGraph (FactBlock (FactBlock), FactBody, FactGraph)
import Oat.Dataflow.FactGraph qualified as FactGraph
import Oat.Dataflow.Graph (Body, Graph, NonLocal (..))
import Oat.Dataflow.Graph qualified as Graph
import Oat.Dataflow.LabelMap (Label, LabelMap)
import Oat.Dataflow.LabelMap qualified as LabelMap
import Oat.Dataflow.LabelSet qualified as LabelSet
import Oat.Dataflow.Shape (IndexedCO, MaybeC (..), MaybeO (..), Shape (..), convertMaybeO)
import Oat.Dataflow.TreeUtils qualified as TreeUtils
import Oat.Utils.Misc (show')
import Oat.Utils.Optics (unwrap)

data ChangeFlag = NoChange | SomeChange
  deriving (Show, Eq, Ord)

type Join f = Label -> FactPair f -> (ChangeFlag, f)

-- transfers must be monotonic
-- given a fact in, we must give a more informative fact out
type ForwardTransfer n f =
  ( (n C O -> f -> f),
    (n O O -> f -> f),
    (n O C -> f -> FactBase f)
  )

data ForwardRewrite es n f
  = ForwardRewrite3
      (n C O -> f -> Eff es (Maybe (Graph n C O, ForwardRewrite es n f)))
      (n O O -> f -> Eff es (Maybe (Graph n O O, ForwardRewrite es n f)))
      (n O C -> f -> Eff es (Maybe (Graph n O C, ForwardRewrite es n f)))

data ForwardPass es n f = ForwardPass
  { lattice :: !(Lattice f),
    transfer :: ForwardTransfer n f,
    rewrite :: ForwardRewrite es n f
  }

data BackwardTransfer n f
  = BackwardTransfer3
      (n C O -> f -> f)
      (n O O -> f -> f)
      (n O C -> FactBase f -> f)

data BackwardRewrite es n f
  = BackwardRewrite3
      (n C O -> f -> Eff es (Maybe (Graph n C O, BackwardRewrite es n f)))
      (n O O -> f -> Eff es (Maybe (Graph n O O, BackwardRewrite es n f)))
      (n O C -> FactBase f -> Eff es (Maybe (Graph n O C, BackwardRewrite es n f)))

data BackwardPass es n f = BackwardPass
  { lattice :: !(Lattice f),
    transfer :: BackwardTransfer n f,
    rewrite :: BackwardRewrite es n f
  }

data Direction = Forward | Backward
  deriving (Show, Eq)

type Fact ex f = IndexedCO ex (FactBase f) f

data Lattice f = Lattice
  { name :: !Text,
    bot :: !f,
    join :: Join f
  }

type FactBase f = LabelMap f

data FactPair f = FactPair
  { old :: !f,
    new :: !f
  }

changeIf :: Bool -> ChangeFlag
changeIf changed = if changed then SomeChange else NoChange

runForward ::
  forall es n f e x.
  (forall e x. (Show (n e x)), Show f) =>
  (NonLocal n) =>
  ForwardPass es n f ->
  MaybeC e Label ->
  Graph n e x ->
  Fact e f ->
  Eff es (FactGraph f n e x, Fact x f)
runForward pass start = graph
  where
    graph :: Graph n e x -> Fact e f -> Eff es (FactGraph f n e x, Fact x f)
    graph Graph.Empty = \f -> pure (Graph.Empty, f)
    graph (Graph.Single b) = block b
    graph (Graph.Many e bdy x) = (e `entryBodyCompose` bdy) `compose` exit x
      where
        maybeExitLabel = entryLabel <$> convertMaybeO x

        exit :: MaybeO x (Block n C O) -> FactBase f -> Eff es (FactGraph f n C x, Fact x f)
        exit (JustO blk) = \factBase ->
          block blk (getFact pass.lattice (entryLabel blk) factBase)
        exit NothingO = \fb -> pure (Graph.emptyClosed, fb)

        entryBodyCompose ::
          (MaybeO e (Block n O C)) ->
          Body n ->
          Fact e f ->
          Eff es (FactGraph f n e C, Fact C f)
        entryBodyCompose entry bdy = case (start, entry) of
          (NothingC, JustO entry) ->
            block entry `compose` body (successorLabels entry) bdy
          (JustC start, NothingO) -> body [start] bdy
          where
            body :: [Label] -> Body n -> FactBase f -> Eff es (FactGraph f n C C, FactBase f)
            body start body initFactBase =
              -- we always return the graph with our facts for the block,
              -- and outgoing facts that include facts for successors
              fixpoint Forward pass.lattice doBlock (start, maybeExitLabel) body initFactBase
              where
                doBlock :: Block n C C -> FactBase f -> Eff es (FactGraph f n C C, FactBase f)
                doBlock b fb =
                  -- get our fact that predecessors have given us
                  -- or bottom if they haven't given us anything
                  block b $ getFact pass.lattice (entryLabel b) fb

    block :: forall e x. Block n e x -> f -> Eff es (FactGraph f n e x, Fact x f)
    block (Block.CO n b) = node n `compose` block b
    block (Block.CC n b n') = node n `compose` block b `compose` node n'
    block (Block.OC b n) = block b `compose` node n
    block (Block.OO bs) = \f ->
      Acc.foldlM
        (\prev@(_, !f) n -> node n f <&> combine prev)
        (Graph.Empty, f)
        bs

    node ::
      forall e x.
      (ShapeLifter e x) =>
      n e x ->
      f ->
      Eff es (FactGraph f n e x, Fact x f)
    node n = \f -> do
      rewrittenGraph <- forwardRewrite pass.rewrite n f
      case rewrittenGraph of
        Nothing -> pure (singletonFactGraph f n, forwardTransfer pass.transfer n f)
        Just (g, rewrite) -> do
          let pass' = (pass {rewrite} :: ForwardPass es n f)
              f' = forwardEntryFact n f
          runForward pass' (forwardEntryLabel n) g f'

    combine ::
      forall e a x f1 f2.
      (FactGraph f n e a, f1) ->
      (FactGraph f n a x, f2) ->
      (FactGraph f n e x, f2)
    combine (!g1, !_f1) (!g2, !f2) = (g1 `FactGraph.spliceRight` g2, f2)

    compose ::
      forall e a x f1 f2 f3.
      (f1 -> Eff es (FactGraph f n e a, f2)) ->
      (f2 -> Eff es (FactGraph f n a x, f3)) ->
      (f1 -> Eff es (FactGraph f n e x, f3))
    compose trans trans' = \f -> do
      res1@(_, f1) <- trans f
      res2 <- trans' f1
      pure $ combine res1 res2
    {-# INLINE compose #-}

runBackward ::
  forall es n f e x.
  (forall e x. Show (n e x), Show f) =>
  (NonLocal n) =>
  -- the backward pass to use
  BackwardPass es n f ->
  -- when e ~ C, the entry label
  -- when e ~ O, Nothing because we will always choose the bodys and then the anonymous entry block
  MaybeC e Label ->
  Graph n e x ->
  -- when x ~ C, the starting facts for the transfer function to read in
  -- when x ~ O, just the starting facts for the transfer function
  Fact x f ->
  -- when e ~ C, we get a factbase
  -- when e ~ O, we get the out facts of the entry block
  Eff es (FactGraph f n e x, Fact e f)
runBackward pass start = graph
  where
    graph :: Graph n e x -> Fact x f -> Eff es (FactGraph f n e x, Fact e f)
    graph Graph.Empty = \f -> pure (Graph.empty, f)
    graph (Graph.Single blk) = block blk
    graph (Graph.Many e bdy x) = (e `entryBodyCompose` bdy) `compose` exit x
      where
        maybeExitLabel = entryLabel <$> convertMaybeO x

        exit :: MaybeO x (Block n C O) -> Fact x f -> Eff es (FactGraph f n C x, Fact C f)
        exit (JustO blk) = \f -> do
          (rg, f) <- block blk f
          let fb = LabelMap.singleton (entryLabel blk) f
          pure (rg, fb)
        exit NothingO = \fb -> pure (Graph.emptyClosed, fb)

        entryBodyCompose :: MaybeO e (Block n O C) -> Body n -> Fact C f -> Eff es (FactGraph f n e C, Fact e f)
        entryBodyCompose entry bdy = case (start, entry) of
          (NothingC, JustO entry) -> block entry `compose` body (successorLabels entry) bdy
          (JustC start, NothingO) -> body [start] bdy
          where
            body :: [Label] -> Body n -> Fact C f -> Eff es (FactGraph f n C C, Fact C f)
            body starts bdy initFactBase = do
              fixpoint Backward pass.lattice doBlock (starts, maybeExitLabel) bdy initFactBase
              where
                doBlock :: Block n C C -> FactBase f -> Eff es (FactGraph f n C C, FactBase f)
                doBlock b f = do
                  -- the out facts are the same as the facts in the graph
                  (g, f) <- block b f
                  -- name the facts with the block entry label
                  pure (g, LabelMap.singleton (entryLabel b) f)

    -- Lift from nodes to blocks
    block :: forall e x. Block n e x -> Fact x f -> Eff es (FactGraph f n e x, f)
    block (Block.CO n b) = node n `compose` block b
    block (Block.CC n b n') = node n `compose` block b `compose` node n'
    block (Block.OC b n) = block b `compose` node n
    block (Block.OO ns) = \f ->
      Acc.foldrM
        (\n prev@(_, f) -> node n f <&> combine prev)
        (Graph.Empty, f)
        ns

    node ::
      forall e x.
      Show (Fact x f) =>
      (ShapeLifter e x) =>
      n e x ->
      Fact x f ->
      Eff es (FactGraph f n e x, f)
    node n f = do
      let !_ = dbg "f"
      let !_ = dbg f
      let !_ = dbg "about to rewrite"
      let !_ = dbg n
      rewritten <- backwardRewrite pass.rewrite n f
      let !_ = dbg $ "rewritten: " ++ show' (fst <$> rewritten)
      case rewritten of
        Nothing -> do
          let !_ = dbg "entryF"
          let !_ = dbg entryF
          pure (singletonFactGraph entryF n, entryF)
          where
            entryF = backwardTransfer pass.transfer n f
        Just (g, rewrite) -> do
          let pass' = (pass {rewrite} :: BackwardPass es n f)
          -- recursively analyze and rewrite the rewritten graph
          -- if the node is n C O, pass in the starting label so we know where to start
          -- pass nothing for O O and O C
          (g, f) <- runBackward pass' (forwardEntryLabel n) g f
          pure
            ( g,
              -- get our block from the factbase if necessary
              -- if the node is n C O, get our fact from the factbase
              backwardEntryFact pass.lattice n f
            )

    combine (!g2, !_f2) (!g1, !f1) = (g1 `FactGraph.spliceLeft` g2, f1)

    -- Compose fact transformers and concatentate the resulting
    -- rewritten graphs.
    compose ::
      forall e a x info info' info''.
      (info' -> Eff es (FactGraph f n e a, info'')) ->
      (info -> Eff es (FactGraph f n a x, info')) ->
      (info -> Eff es (FactGraph f n e x, info''))
    compose trans trans' f = do
      res2@(_, f2) <- trans' f
      res1 <- trans f2
      pure $ combine res2 res1
    {-# INLINE compose #-}

makeFactBase :: Lattice f -> [(Label, f)] -> FactBase f
makeFactBase lattice = foldl' add mempty
  where
    add map (l, !new) = map & at l ?~ newFact
      where
        newFact = case map ^. at l of
          Nothing -> new
          Just old -> snd $ lattice.join l FactPair {old, new}

fixpoint ::
  forall es n f.
  (forall e x. Show (n e x), Show f) =>
  (NonLocal n) =>
  -- which direction to do it in?
  Direction ->
  -- the lattice to use
  Lattice f ->
  -- the function to rewrite and then analyze each block by giving in input facts
  (Block n C C -> FactBase f -> Eff es (FactGraph f n C C, FactBase f)) ->
  -- the start labels and the label of the last block not included in the body
  -- we want to make sure we don't access it
  ([Label], Maybe Label) ->
  -- the body
  Body n ->
  -- the starting factbase
  FactBase f ->
  Eff es (FactGraph f n C C, FactBase f)
fixpoint direction lattice doBlock (entries, maybeExitLabel) body factBase = do
  let !_ = dbg ("exit label " ++ show maybeExitLabel)
  let !_ = dbg ("fixpoint: " ++ show direction)
  let !_ = dbg "body"
  let !_ = dbg body
  let !_ = dbg "entries"
  let !_ = dbg entries
  let !_ = dbg "factBase"
  let !_ = dbg factBase
  let !_ = dbg "ordered"
  let !_ = dbg ordered
  (factBase', newBlocks) <- loop factBase (fromList ordered) (mempty @(LabelMap _))
  let !_ = dbg ("fixpoint DONE: ")
  let !_ = dbg "factBase'"
  let !_ = dbg factBase'
  let !_ = dbg "newBlocks"
  let !_ = dbg newBlocks
  pure (Graph.fromBody newBlocks, factBase')
  where
    ordered = case direction of
      -- forwards fixpoint is driven by the successors that have changed
      Forward -> entries
      -- backwards fixpoint is driven by dfs
      Backward -> do
        let entries' = filterExitLabel entries
        let pruned =
              Graph.pruneWith (foldMap LabelSet.singleton maybeExitLabel)
                . Graph.generates (filterExitLabel entries)
                $ body
        let !_ = dbg "entrie'"
        let !_ = dbg entries'
        let !_ = dbg "pruned"
        let !_ = dbg pruned
        TreeUtils.postOrderF pruned

    -- we need to make sure that we don't access the exit label
    -- which is not in the label map body
    filterExitLabel = filter ((/=) maybeExitLabel . Just)

    -- mapping from L -> Ls.  If the fact for L changes, re-analyse Ls.
    -- if the direction if forward, we just wrap the label in a list
    -- this is okay because the changed labels returned after doBlock *are* the successors
    -- so we don't have to do any extra work
    -- if we are backward, doBlock only will return one changed label, which is the current label
    -- we need to do some work to get the predecessors
    -- we get the predecessors finding the successors of every block
    blockDeps = case direction of
      Forward -> Just . filterExitLabel . pure @[]
      Backward -> \label -> backwardDeps ^. at label
      where
        backwardDeps =
          LabelMap.fromListWith
            (++)
            [ (l, [entryLabel b])
              | b <- snd <$> toList body,
                l <- successorLabels b
            ]

    loop ::
      -- current factbase (increases monotonically)
      FactBase f ->
      -- the worklist queue of blocks that we still need to analyze
      Seq Label ->
      -- the rewritten graph which gets more and more precise after each iteration
      FactBody f n ->
      Eff es (FactBase f, LabelMap (FactBlock f n C C))
    loop factBase Seq.Empty newBlocks = pure (factBase, newBlocks)
    loop factBase (lab Seq.:<| labTodo) newBlocks = do
      -- we use the original body here, not the rewritten one
      -- if we do an incorrect rewrite, we can still be okay with the original one
      case body ^. at lab of
        Nothing -> loop factBase labTodo newBlocks
        Just block -> do
          (resultFactGraph, outFacts) <- doBlock block factBase
          let (changed, factBase') =
                LabelMap.foldrWithKey'
                  (updateFact lattice newBlocks)
                  ([], factBase)
                  outFacts
          let toAnalyze =
                concatMap
                  ( \l ->
                      (^. unwrap (error $ "The label " ++ show l ++ " was said to be changed but could not be found")) $
                        blockDeps l
                  )
                  $ changed
          let newBlocks' = case resultFactGraph of
                -- this union is biased to the left
                -- this is important because we may have more precise rewrites
                -- we want them to overwrite them ones on the right
                Graph.Many _ blocks _ -> LabelMap.union blocks newBlocks
          loop factBase' (labTodo <> fromList toAnalyze) newBlocks'

updateFact ::
  Lattice f ->
  LabelMap (FactBlock f n C C) ->
  Label ->
  f ->
  ([Label], FactBase f) ->
  ([Label], FactBase f)
updateFact lattice newBlocks lab newFact (changed, !factBase)
  -- make sure that we do not skip blocks that have not been seen yet
  -- forward running is driven by bfs, so we need to make sure that we add the block
  | NoChange <- changeFlag, has (ix lab) newBlocks = (changed, factBase)
  | otherwise = (lab : changed, factBase & at lab ?~ resultFact)
  where
    (changeFlag, resultFact) = case factBase ^. at lab of
      Nothing -> (SomeChange, newFactDebug)
      Just oldFact -> join oldFact
    join oldFact = lattice.join lab FactPair {new = newFact, old = oldFact}
    (_, newFactDebug) = join lattice.bot

-- Lifting based on shape:
--  - from nodes to blocks
--  - from facts to fact-like things
-- Lowering back:
--  - from fact-like things to facts
-- Note that the latter two functions depend only on the entry shape.
class ShapeLifter e x where
  singletonFactGraph :: f -> n e x -> FactGraph f n e x
  forwardEntryFact :: NonLocal n => n e x -> f -> Fact e f
  forwardEntryLabel :: NonLocal n => n e x -> MaybeC e Label
  forwardTransfer :: ForwardTransfer n f -> n e x -> f -> Fact x f
  forwardRewrite :: ForwardRewrite es n f -> n e x -> f -> Eff es (Maybe (Graph n e x, ForwardRewrite es n f))
  backwardEntryFact :: NonLocal n => Lattice f -> n e x -> Fact e f -> f
  backwardTransfer :: BackwardTransfer n f -> n e x -> Fact x f -> f
  backwardRewrite :: BackwardRewrite es n f -> n e x -> Fact x f -> Eff es (Maybe (Graph n e x, BackwardRewrite es n f))

instance ShapeLifter C O where
  singletonFactGraph fact node = Graph.exit (FactBlock fact (Block.CO node Block.empty))
  forwardEntryFact node fact = LabelMap.singleton (entryLabel node) fact
  forwardEntryLabel node = JustC $ entryLabel node
  forwardTransfer = (^. _1)
  forwardRewrite (ForwardRewrite3 rewrite _ _) = rewrite
  backwardEntryFact lattice node factBase = getFact lattice (entryLabel node) factBase
  backwardTransfer (BackwardTransfer3 transfer _ _) = transfer
  backwardRewrite (BackwardRewrite3 rewrite _ _) = rewrite

instance ShapeLifter O O where
  singletonFactGraph fact = Graph.single . FactBlock fact . Block.singleton
  forwardEntryFact _ f = f
  forwardEntryLabel _ = NothingC
  forwardTransfer = (^. _2)
  forwardRewrite (ForwardRewrite3 _ rewrite _) = rewrite
  backwardEntryFact _ _ fact = fact
  backwardTransfer (BackwardTransfer3 _ transfer _) = transfer
  backwardRewrite (BackwardRewrite3 _ rewrite _) = rewrite

instance ShapeLifter O C where
  singletonFactGraph fact node = Graph.entry (FactBlock fact (Block.OC Block.empty node))
  forwardEntryFact _ fact = fact
  forwardEntryLabel _ = NothingC
  forwardTransfer = (^. _3)
  forwardRewrite (ForwardRewrite3 _ _ rewrite) = rewrite
  backwardEntryFact _ _ fact = fact
  backwardTransfer (BackwardTransfer3 _ _ transfer) = transfer
  backwardRewrite (BackwardRewrite3 _ _ rewrite) = rewrite

getFact :: Lattice f -> Label -> FactBase f -> f
getFact lattice label factBase = factBase ^. at label % unwrap lattice.bot

backwardTransfer1 :: (forall e x. n e x -> Fact x f -> f) -> BackwardTransfer n f
backwardTransfer1 f = BackwardTransfer3 f f f

backwardRewrite3 ::
  forall es n f.
  (n C O -> f -> Eff es (Maybe (Graph n C O))) ->
  (n O O -> f -> Eff es (Maybe (Graph n O O))) ->
  (n O C -> FactBase f -> Eff es (Maybe (Graph n O C))) ->
  BackwardRewrite es n f
backwardRewrite3 f m l = BackwardRewrite3 (lift f) (lift m) (lift l)
  where
    lift ::
      forall n e x.
      (n e x -> Fact x f -> Eff es (Maybe (Graph n e x))) ->
      n e x ->
      Fact x f ->
      Eff es (Maybe (Graph n e x, BackwardRewrite es n f))
    lift rewrite node fact = (fmap . fmap) (,noBackwardRewrite) (rewrite node fact)

noBackwardRewrite :: BackwardRewrite es n f
noBackwardRewrite = BackwardRewrite3 noRewrite noRewrite noRewrite

backwardRewrite1 ::
  (forall e x. n e x -> Fact x f -> Eff es (Maybe (Graph n e x))) ->
  BackwardRewrite es n f
backwardRewrite1 f = backwardRewrite3 f f f

noRewrite :: a -> b -> Eff es (Maybe c)
noRewrite _ _ = pure Nothing
