{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.ToTree where

import qualified Data.HashMap.Optics as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.Range (Range (RangeP))
import Oat.Common (inBetween)
import Oat.LL.AST
import Oat.LL.Name (Name)
import Optics
import Optics.Operators.Unsafe ((^?!))

data ToTreeState = ToTreeState
  { idToInst :: !(IntMap Inst),
    nameToInst :: !(HashMap Name (Int, Inst)),
    idToUses :: !(IntMap Int),
    term :: !Term
  }

$(makeFieldLabelsNoPrefix ''ToTreeState)

type MonadToTree = MonadState ToTreeState

type ToTreeM = State ToTreeState

toTreeInst :: MonadToTree m => Int -> Inst -> m Inst
toTreeInst id inst = do
  st <- get
  case toTreeInst' id inst st of
    Just (toDelete, inst') -> undefined
    Nothing -> pure inst

-- case toTreeInst'
-- let (toDelete, inst')

toTreeInst' :: Int -> Inst -> ToTreeState -> Maybe (Int, Inst)
toTreeInst' id inst st = do
  -- TODO: for now stuff without names cannot be combined
  name <- inst ^? instName
  useId <- st ^. #idToUses % at id
  let useInst = st ^?! #idToInst % ix useId
  let between = inBetween (RangeP id useId) (st ^. #idToInst)
  let substDownRes = (id, substInstInst inst useInst)
  case (getMoveStatus inst, getMoveStatus useInst) of
    (EasilyMoved, _) -> Just substDownRes
    (HardToMove, _) ->
      if usesName name between
        then Nothing
        else case inst of
          Call _ | noCalls between -> Just substDownRes
          _ -> Nothing
    _ -> Nothing

noCalls :: IntMap Inst -> Bool
noCalls = allOf each (hasn't #_Call)

usesName :: Name -> IntMap Inst -> Bool
usesName name = allOf (each % instOperands % operandNames) (/= name)

-- between = inBetween idT

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

-- usesInBetween :: Name -> Range -> ToTreeState -> Bool
-- usesInBetween name range@(RangeP start end) st = undefined
--   where
--     between = inBetween idT

stateFromBlock :: Block -> ToTreeState
stateFromBlock block =
  ToTreeState
    { idToInst,
      nameToInst,
      idToUses,
      term = block ^. #term
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
            names = inst ^.. instOperands % operandNames
            ids = (\name -> nameToInst ^?! at name % _Just % _1) <$> names
            first = getFirst $ foldMap (First . Just) ids
            firstSingle = case ids of
              _ : _ : _ -> Nothing
              _ -> first