{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Oat.Dataflow.Graph
  ( Body,
    Body',
    emptyBody,
    Graph,
    Graph' (..),
    NonLocal (..),
    fromBody,
    bodyUnion,
    splice',
    splice,
    spliceClosed,
    dfs,
    dfsBody,
    append,
    entry,
    exit,
    first,
    last,
    middle,
    single,
    emptyClosed,
    empty,
    block,
    prune,
    pruneWith,
    generate,
    generates,
    (><),
    mapBlocks3,
    MapBlocks3,
    mapBlocks,
  )
where

import Control.Monad.State.Strict (State, evalState, get)
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Oat.Dataflow.Block (Block, BlockK, Node)
import Oat.Dataflow.Block qualified as Block
import Oat.Dataflow.LabelMap (Label, LabelMap)
import Oat.Dataflow.LabelMap qualified as LabelMap
import Oat.Dataflow.LabelSet (LabelSet)
import Oat.Dataflow.LabelSet qualified as LabelSet
import Oat.Dataflow.Shape (MaybeC (..), MaybeO (..), Shape (..), convertMaybeO)
import Oat.Utils.Misc ((<<$>>))
import Oat.Utils.Optics (unwrap)
import Optics.State.Operators ((?=))
import Prelude hiding (Empty, empty, first, last)

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

addFactBlock :: NonLocal thing => thing C C -> LabelMap (thing C C) -> LabelMap (thing C C)
addFactBlock b body
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
    { entry :: !(MaybeO e (block n O C)),
      body :: !(Body' block n),
      exit :: !(MaybeO x (block n C O))
    } ->
    Graph' block n e x

deriving instance (forall e x. Show (block n e x)) => (Show (Graph' block n e x))

deriving instance (forall e x. Eq (block n e x)) => (Eq (Graph' block n e x))

class NonLocal n where
  entryLabel :: n C x -> Label
  successorLabels :: n e C -> [Label]

instance NonLocal n => NonLocal (Block n) where
  entryLabel (Block.CO n _) = entryLabel n
  entryLabel (Block.CC n _ _) = entryLabel n

  successorLabels (Block.OC _ n) = successorLabels n
  successorLabels (Block.CC _ _ n) = successorLabels n

fromBody :: Body' block n -> Graph' block n C C
fromBody b = Many NothingO b NothingO

type BlockCatFun block n = forall e x. block n e O -> block n O x -> block n e x

splice' ::
  forall block n e a x.
  NonLocal (block n) =>
  BlockCatFun block n ->
  ( Graph' block n e a ->
    Graph' block n a x ->
    Graph' block n e x
  )
splice' bcat = sp
  where
    sp :: forall e a x. Graph' block n e a -> Graph' block n a x -> Graph' block n e x
    sp Empty g = g
    sp g Empty = g
    sp (Single b) (Single b') = Single $ b `bcat` b'
    sp (Single b) (Many (JustO e) bs x) = Many (JustO (b `bcat` e)) bs x
    sp (Many e bs (JustO x)) (Single b) = Many e bs (JustO (x `bcat` b))
    sp (Many e bs (JustO x)) (Many (JustO e') bs' x') = Many e ((addFactBlock (x `bcat` e') bs) `bodyUnion` bs') x'
    sp (Many e b NothingO) (Many NothingO b' x) = Many e (b `bodyUnion` b') x

empty :: Graph' block n O O
empty = Empty

emptyClosed :: Graph' block n C C
emptyClosed = Many NothingO emptyBody NothingO

last :: n O C -> Graph n O C
last n = entry $ Block.OC Block.empty n

first :: n C O -> Graph n C O
first n = exit $ Block.CO n Block.empty

class FromBlock e x block n where
  block :: block n e x -> Graph' block n e x

instance FromBlock O C block n where
  block b = Many (JustO b) emptyBody NothingO

instance FromBlock C O block n where
  block b = Many NothingO emptyBody (JustO b)

instance NonLocal (block n) => FromBlock C C block n where
  block b = Many NothingO (addFactBlock b emptyBody) NothingO

instance FromBlock O O block n where
  block = Single

entry :: block n O C -> Graph' block n O C
entry block = Many (JustO block) emptyBody NothingO

exit :: block n C O -> Graph' block n C O
exit block = Many NothingO emptyBody (JustO block)

middle :: (NonLocal (block n)) => block n C C -> Graph' block n C C
middle b = Many NothingO (addFactBlock b emptyBody) NothingO

single :: block n 'O 'O -> Graph' block n 'O 'O
single = Single

-- | append two graphs, control flow flows from left to right
append :: NonLocal n => Graph n e O -> Graph n O x -> Graph n e x
append = splice

-- | splice two graphs, nothing is known about control flow
spliceClosed :: NonLocal n => Graph n e C -> Graph n C x -> Graph n e x
spliceClosed = splice

splice :: NonLocal n => Graph n e a -> Graph n a x -> Graph n e x
splice = splice' Block.append

(><) :: NonLocal n => Graph n e a -> Graph n a x -> Graph n e x
(><) = splice

infixl 5 ><

dfs :: NonLocal (block n) => MaybeC e [Label] -> Graph' block n e x -> [Tree (block n C C)]
dfs NothingC Empty = []
dfs NothingC Single {} = []
dfs mEntries (Many entry body exit) = (\l -> body ^. at l % unwrap (error "label was not found in graph")) <<$>> labels
  where
    labels = dfsBody seen entries body

    entries :: [Label]
    entries = case (mEntries, entry) of
      (NothingC, JustO entry) -> successorLabels entry
      (JustC entries, NothingO) -> entries

    seen = foldMap LabelSet.singleton $ entryLabel <$> convertMaybeO exit

dfsBody :: NonLocal (block n) => LabelSet -> [Label] -> Body' block n -> [Tree Label]
dfsBody seen entries = pruneWith seen . generates entries

generates :: NonLocal (block n) => [Label] -> Body' block n -> [Tree Label]
generates entries body = (`generate` body) <$> entries

generate :: NonLocal (block n) => Label -> Body' block n -> Tree Label
generate entry body = Tree.Node entry ((`generate` body) <$> (successorLabels entryBlock))
  where
    entryBlock =
      body ^. at entry
        % unwrap
          ( error $
              "The label "
                ++ show entry
                ++ " was not found in the body"
          )

prune :: [Tree Label] -> [Tree Label]
prune = pruneWith (mempty @LabelSet)

pruneWith :: LabelSet -> [Tree Label] -> [Tree Label]
pruneWith seen forest = evalState (chop forest) seen

chop :: [Tree Label] -> State LabelSet [Tree Label]
chop [] = pure []
chop (Tree.Node label blocks : blockForest) = do
  seen <- get
  if has (ix label) seen
    then chop blockForest
    else do
      at label ?= ()
      block' <- chop blocks
      blockForest' <- chop blockForest
      pure $ Tree.Node label block' : blockForest'

type MapBlocks3 block block' n =
  ( block n O C -> block' n O C,
    block n O O -> block' n O O,
    block n C C -> block' n C C,
    block n C O -> block' n C O
  )

mapBlocks3 ::
  forall block block' n e x.
  MapBlocks3 block block' n ->
  Graph' block n e x ->
  Graph' block' n e x
mapBlocks3 (mapEntry, mapSingle, mapMiddle, mapExit) = \case
  Empty -> Empty
  Single b -> Single (mapSingle b)
  Many entry body exit -> Many (mapEntry <$> entry) (mapMiddle <$> body) (mapExit <$> exit)

mapBlocks ::
  forall block block' n e x.
  (forall e x. block n e x -> block' n e x) ->
  Graph' block n e x ->
  Graph' block' n e x
mapBlocks f = mapBlocks3 (f, f, f, f)
