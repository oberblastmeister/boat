{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.Combine where

import Data.HashMap.Optics qualified as HashMap
import Data.HashSet.InsOrd (InsOrdHashSet)
import Data.HashSet.Optics qualified as HashSet
import Data.IntMap.Strict qualified as IntMap
import Data.Range (Range (RangeP))
import Oat.Common (inBetween, insOrdSetOf)
import Oat.LL.AST qualified as LL
import Oat.LL.Name (Name)
import Optics.Operators.Unsafe ((^?!))

data CombineState = CombineState
  { -- this serves to purposes:
    -- to be a map from ids to Insts
    -- and also to be an ordered representation of the program
    -- thus when we combine instructions we have to delete the ids from this field
    -- the other fields are just used for maps only
    idToInst :: !(IntMap LL.Inst),
    nameToInst :: !(HashMap Name (Int, LL.Inst)),
    -- if it is Nothing, that means it is used in a terminator
    idToUses :: !(IntMap Int),
    term :: !LL.Term,
    prevId :: !Int
  }

$(makeFieldLabelsNoPrefix ''CombineState)

type MonadCombine = MonadState CombineState

type CombineM = State CombineState

-- combineBody :: LL.FunBody -> LL.FunBody
-- combineBody body =
--   where
--     body' = removeAlloca

combineBlock :: LL.Block -> LL.Block
combineBlock block = block'
  where
    block' = LL.Block {insts = combineState' ^.. #idToInst % each, term = combineState' ^. #term}
    ((), combineState') = runState combineLoop combineState
    combineState = stateFromBlock block

combineLoop :: MonadCombine m => m ()
combineLoop = do
  prevId <- use #prevId
  idToInst <- use #idToInst
  case IntMap.lookupGT prevId idToInst of
    Just (currId, inst) -> do
      combineInst currId inst
      #prevId .= currId
      combineLoop
    Nothing -> pure ()

combineInst :: MonadCombine m => Int -> LL.Inst -> m ()
combineInst id inst = do
  st <- get
  case combineInst' id inst st of
    Just (toDelete, toInsert, inst') -> do
      #idToInst % at toDelete .= Nothing
      #idToInst % at toInsert ?= inst'
    Nothing -> pure ()

combineInst' :: Int -> LL.Inst -> CombineState -> Maybe (Int, Int, LL.Inst)
combineInst' id inst st = do
  -- TODO: for now stuff without names cannot be combined
  name <- inst ^? LL.instName
  useId <- st ^. #idToUses % at id
  let useInst = st ^?! #idToInst % ix useId
  -- for now if the next instructions uses the name twice or more, we cannot combine it
  -- maybe we should have some sort of seq instruction
  case useInst ^.. LL.instOperands % filtered (\operand -> Just name == (operand ^? LL.operandName)) of
    _ : _ : _ -> Nothing
    _ -> pure ()
  let between = inBetween (RangeP id useId) (st ^. #idToInst)
  let substDownRes = (id, useId, substInstInst inst useInst)
  case (getMoveStatus inst, getMoveStatus useInst) of
    (EasilyMoved, _) -> Just substDownRes
    (HardToMove, _) ->
      if usesName name between
        then Nothing
        else case inst of
          LL.Call _
            | noCalls between -> Just substDownRes
            | otherwise -> Nothing
          _ -> Just substDownRes
    _ -> Nothing

noCalls :: IntMap LL.Inst -> Bool
noCalls = allOf each (hasn't #_Call)

usesName :: Name -> IntMap LL.Inst -> Bool
usesName name = allOf (each % LL.instOperands % LL.operandName) (/= name)

substInstInst :: LL.Inst -> LL.Inst -> LL.Inst
substInstInst inst inst' = inst' & LL.instOperands %~ substInstOperand inst

substInstOperand :: LL.Inst -> LL.Operand -> LL.Operand
substInstOperand inst operand = case (inst ^? LL.instName, operand) of
  (Just name, LL.Temp name') | name == name' -> LL.Nested inst
  (Just _name, LL.Nested inst') -> LL.Nested $ inst' & LL.instOperands %~ substInstOperand inst
  (Just _name, other) -> other
  (Nothing, other) -> other

-- for calls we also need to make sure that there aren't any calls in between
getMoveStatus :: LL.Inst -> MoveStatus
getMoveStatus = \case
  LL.Alloca _ -> Don'tMove
  LL.Call _ -> HardToMove
  LL.Store _ -> HardToMove
  LL.Load _ -> HardToMove
  LL.Gep _ -> EasilyMoved
  LL.BinOp _ -> EasilyMoved
  LL.Bitcast _ -> EasilyMoved
  LL.Icmp _ -> EasilyMoved

data MoveStatus
  = EasilyMoved
  | Don'tMove
  | HardToMove

-- usesInBetween :: Name -> Range -> CombineState -> Bool
-- usesInBetween name range@(RangeP start end) st = undefined
--   where
--     between = inBetween idT

stateFromBlock :: LL.Block -> CombineState
stateFromBlock block =
  CombineState
    { idToInst,
      nameToInst,
      idToUses,
      term = block ^. #term,
      prevId = 0
    }
  where
    idToInst = IntMap.fromList instWithId
    nameToInst = HashMap.toMapOf (folded % _Just % ifolded) instWithName
    instWithName =
      ( \(id, inst) -> do
          name <- inst ^? LL.instName
          pure (name, (id, inst))
      )
        <$> instWithId
    instWithId = zip [1 :: Int ..] (block ^. #insts)
    idToUses = foldl' go Empty instWithId
      where
        go :: IntMap Int -> (Int, LL.Inst) -> IntMap Int
        go uses (id, inst) = uses & at id .~ firstSingle
          where
            names = inst ^.. LL.instOperands % LL.operandName
            ids = (\name -> nameToInst ^?! at name % _Just % _1) <$> names
            first = getFirst $ foldMap (First . Just) ids
            firstSingle = case ids of
              _ : _ : _ -> Nothing
              _ -> first

-- probably not the fastest
-- removeAlloca :: LL.FunBody -> (LL.FunBody, InsOrdHashSet Name)
-- removeAlloca body = undefined
--   where
--     -- body' =
--     --   body & LL.bodyInsts % LL.instOperands
--     --     %~ ( \case
--     --            LL.Temp name | has (ix name) allocaNameSet -> LL.MemTemp name
--     --            other -> other
--     --        )
--     body' = foldlOf' LL.bodyInsts (_) (mempty)
--       body & LL.bodyInsts %~ (\case
--       )
--         -- %~ ( \case
--         --        LL.Temp name | has (ix name) allocaNameSet -> LL.MemTemp name
--         --        other -> other
--         --    )
--     -- allocaNameSet = insOrdSetOf (LL.bodyInsts % #_Alloca % #name) body