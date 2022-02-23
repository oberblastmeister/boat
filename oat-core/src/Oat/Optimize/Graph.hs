{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Optimize.Graph
  ( Body,
    Body',
    emptyBody,
    Graph,
    Graph' (..),
    NonLocal (..),
    fromBody,
    bodyUnion,
    spliceAppend',
    splice,
    spliceAppend,
    dfs,
    dfsFrom,
    append,
    entry,
    exit,
    first,
    last,
    middle,
  )
where

import Control.Monad.State.Strict (State, evalState, get)
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Oat.Optimize.Block (Block, BlockK, MaybeO (..), Node, Shape (..))
import Oat.Optimize.Block qualified as Block
import Oat.Optimize.LabelMap (Label, LabelMap)
import Oat.Optimize.LabelMap qualified as LabelMap
import Oat.Optimize.LabelSet (LabelSet)
import Oat.Utils.Optics (unwrap)
import Optics.State.Operators ((?=))
import Prelude hiding (Empty, first, last)

-- | A (possible empty) collection of closed/closed blocks
type Body :: Node -> Type
type Body n = LabelMap (Block n C C)

-- | @Body@ abstracted over @block@
type Body' :: BlockK -> Node -> Type
type Body' block n = LabelMap (block n C C)

emptyBody :: Body' block n
emptyBody = mempty

bodyUnion :: Body' block n -> Body' block n -> Body' block n
bodyUnion = LabelMap.unionWithKey (\l _ _ -> error $ "duplicate blocks with label " ++ show l)

addBlock :: NonLocal thing => thing C C -> LabelMap (thing C C) -> LabelMap (thing C C)
addBlock b body
  | has (ix l) body = error $ "duplicate label " ++ show l ++ " in graph"
  | otherwise = body & at l ?~ b
  where
    l = entryLabel b

type Graph :: Node -> Shape -> Shape -> Type
type Graph = Graph' Block

data Graph' :: BlockK -> Node -> Shape -> Shape -> Type where
  Empty :: Graph' block n O O
  Single :: !(block n O O) -> Graph' block n O O
  Many ::
    !(MaybeO e (block n O C)) ->
    !(Body' block n) ->
    !(MaybeO x (block n C O)) ->
    Graph' block n e x

class NonLocal n where
  entryLabel :: n C x -> Label
  successorLabels :: n e C -> [Label]

instance NonLocal n => NonLocal (Block n) where
  entryLabel (Block.CO n _) = entryLabel n
  entryLabel (Block.CC n _ _) = entryLabel n

  successorLabels (Block.OC _ n) = successorLabels n
  successorLabels (Block.CC _ _ n) = successorLabels n

fromBody :: Body n -> Graph n C C
fromBody b = Many NothingO b NothingO

type BlockCatFun block n = forall e x. block n e O -> block n O x -> block n e x

spliceAppend' ::
  forall block n e a x.
  NonLocal (block n) =>
  BlockCatFun block n ->
  ( Graph' block n e a ->
    Graph' block n a x ->
    Graph' block n e x
  )
spliceAppend' bcat = sp
  where
    sp :: forall e a x. Graph' block n e a -> Graph' block n a x -> Graph' block n e x
    sp Empty g = g
    sp g Empty = g
    sp (Single b) (Single b') = Single $ b `bcat` b'
    sp (Single b) (Many (JustO e) bs x) = Many (JustO (b `bcat` e)) bs x
    sp (Many e bs (JustO x)) (Single b) = Many e bs (JustO (x `bcat` b))
    sp (Many e bs (JustO x)) (Many (JustO e') bs' x') = Many e ((addBlock (x `bcat` e') bs) `bodyUnion` bs') x'
    sp (Many e b NothingO) (Many NothingO b' x) = Many e (b `bodyUnion` b') x

last :: n O C -> Graph n O C
last n = entry $ Block.OC Block.Empty n

first :: n C O -> Graph n C O
first n = exit $ Block.CO n Block.Empty

entry :: block n O C -> Graph' block n O C
entry block = Many (JustO block) emptyBody NothingO

exit :: block n C O -> Graph' block n C O
exit block = Many NothingO emptyBody (JustO block)

middle :: (NonLocal (block n)) => block n C C -> Graph' block n C C
middle b = Many NothingO (addBlock b emptyBody) NothingO

-- | append two graphs, control flow flows from left to right
append :: NonLocal n => Graph n e O -> Graph n O x -> Graph n e x
append = spliceAppend

-- | splice two graphs, nothing is known about control flow
splice :: NonLocal n => Graph n e C -> Graph n C x -> Graph n e x
splice = spliceAppend

spliceAppend :: NonLocal n => Graph n e a -> Graph n a x -> Graph n e x
spliceAppend = spliceAppend' Block.append

dfs :: NonLocal (block n) => Graph' block n O x -> [Tree Label]
dfs Empty = []
dfs Single {} = []
dfs (Many (JustO entry) body _) = dfs' (successorLabels entry) body

dfsFrom :: NonLocal (block n) => [Label] -> Graph' block n C x -> [Tree Label]
dfsFrom entries (Many NothingO body _) = dfs' entries body

dfs' :: NonLocal (block n) => [Label] -> Body' block n -> [Tree Label]
dfs' entries = prune . generates entries

generates :: NonLocal (block n) => [Label] -> Body' block n -> [Tree Label]
generates entries body = (`generate` body) <$> entries

generate :: NonLocal (block n) => Label -> Body' block n -> Tree Label
generate entry body = Tree.Node entry ((`generate` body) <$> (successorLabels entryBlock))
  where
    entryBlock = (body ^. at entry % unwrap (error $ "The entry " ++ show entry ++ " does not exist in the graph body"))

prune :: [Tree Label] -> [Tree Label]
prune forest = evalState (prune' forest) mempty

prune' :: [Tree Label] -> State LabelSet [Tree Label]
prune' [] = pure []
prune' (Tree.Node label blocks : blockForest) = do
  seen <- get
  if has (ix label) seen
    then prune' blockForest
    else do
      at label ?= ()
      block' <- prune' blocks
      blockForest' <- prune' blockForest
      pure $ Tree.Node label block' : blockForest'

-- toExtra :: Graph n e x -> GraphExtra n e x
-- toExtra graph =
