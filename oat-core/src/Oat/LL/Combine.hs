{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.Combine where

import qualified Data.HashMap.Optics as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.Range (Range (RangeP))
import Oat.Common (inBetween)
import Oat.LL.AST
  ( Block (..),
    Inst (..),
    Operand (Nested, Temp),
    Term,
    instName,
    instOperands,
    operandName,
  )
import Oat.LL.Name (Name)
import Optics.Operators.Unsafe ((^?!))

data CombineState = CombineState
  { -- this serves to purposes:
    -- to be a map from ids to Insts
    -- and also to be an ordered representation of the program
    -- thus when we combine instructions we have to delete the ids from this field
    -- the other fields are just used for maps only
    idToInst :: !(IntMap Inst),
    nameToInst :: !(HashMap Name (Int, Inst)),
    -- if it is Nothing, that means it is used in a terminator
    idToUses :: !(IntMap Int),
    term :: !Term,
    prevId :: !Int
  }

$(makeFieldLabelsNoPrefix ''CombineState)

type MonadCombine = MonadState CombineState

type CombineM = State CombineState

combineBlock :: Block -> Block
combineBlock block = block'
  where
    block' = Block {insts = combineState' ^.. #idToInst % each, term = combineState' ^. #term}
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

combineInst :: MonadCombine m => Int -> Inst -> m ()
combineInst id inst = do
  st <- get
  case combineInst' id inst st of
    Just (toDelete, toInsert, inst') -> do
      #idToInst % at toDelete .= Nothing
      #idToInst % at toInsert ?= inst'
    Nothing -> pure ()

combineInst' :: Int -> Inst -> CombineState -> Maybe (Int, Int, Inst)
combineInst' id inst st = do
  -- TODO: for now stuff without names cannot be combined
  name <- inst ^? instName
  useId <- st ^. #idToUses % at id
  let useInst = st ^?! #idToInst % ix useId
  -- for now if the next instructions uses the name twice or more, we cannot combine it
  -- maybe we should have some sort of seq instruction
  case useInst ^.. instOperands % filtered (\operand -> Just name == (operand ^? operandName)) of
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
          Call _
            | noCalls between -> Just substDownRes
            | otherwise -> Nothing
          _ -> Just substDownRes
    _ -> Nothing

noCalls :: IntMap Inst -> Bool
noCalls = allOf each (hasn't #_Call)

usesName :: Name -> IntMap Inst -> Bool
usesName name = allOf (each % instOperands % operandName) (/= name)

substInstInst :: Inst -> Inst -> Inst
substInstInst inst inst' = inst' & instOperands %~ substInstOperand inst

substInstOperand :: Inst -> Operand -> Operand
substInstOperand inst operand = case (inst ^? instName, operand) of
  (Just name, Temp name') | name == name' -> Nested inst
  (Just _name, Nested inst') -> Nested $ inst' & instOperands %~ substInstOperand inst
  (Just _name, other) -> other
  (Nothing, other) -> other

-- for calls we also need to make sure that there aren't any calls in between
getMoveStatus :: Inst -> MoveStatus
getMoveStatus = \case
  Alloca _ -> Don'tMove
  Call _ -> HardToMove
  Store _ -> HardToMove
  Load _ -> HardToMove
  Gep _ -> EasilyMoved
  BinOp _ -> EasilyMoved
  Bitcast _ -> EasilyMoved
  Icmp _ -> EasilyMoved

data MoveStatus
  = EasilyMoved
  | Don'tMove
  | HardToMove

-- usesInBetween :: Name -> Range -> CombineState -> Bool
-- usesInBetween name range@(RangeP start end) st = undefined
--   where
--     between = inBetween idT

stateFromBlock :: Block -> CombineState
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
          name <- inst ^? instName
          pure (name, (id, inst))
      )
        <$> instWithId
    instWithId = zip [1 :: Int ..] (block ^. #insts)
    idToUses = foldl' go Empty instWithId
      where
        go :: IntMap Int -> (Int, Inst) -> IntMap Int
        go uses (id, inst) = uses & at id .~ firstSingle
          where
            names = inst ^.. instOperands % operandName
            ids = (\name -> nameToInst ^?! at name % _Just % _1) <$> names
            first = getFirst $ foldMap (First . Just) ids
            firstSingle = case ids of
              _ : _ : _ -> Nothing
              _ -> first