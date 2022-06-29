{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Boat.LL.Combine
  ( combineBody,
  )
where

import Control.Parallel.Strategies qualified as Parallel
import Data.HashMap.Optics qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Range (Range (RangeP))
import Data.Vector qualified as V
import Data.Vector.Generic.Optics (toVectorOf)
import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Boat.LL.Ast qualified as LL
import Boat.LL.Name (Name)
import Boat.Utils.Misc (inBetween)
import Boat.Utils.Optics (parOver)
import Optics.Operators.Unsafe ((^?!))

data CombineState = CombineState
  { -- this serves to purposes:
    -- to be a map from ids to Insts
    -- and also to be an ordered representation of the program
    -- thus when we combine instructions we have to delete the ids from this field
    -- the other fields are just used for maps only
    idToInst :: !(IntMap InstWithInfo),
    nameToInst :: !(HashMap Name (Int, InstWithInfo)),
    -- def id to a single use id
    idToUses :: !(IntMap Int),
    term :: !LL.Term,
    prevId :: !Int
  }

data InstWithInfo = InstWithInfo
  { inst :: LL.Inst,
    hasCall :: !Bool,
    hasMem :: !Bool
  }

$(makeFieldLabelsNoPrefix ''CombineState)
$(makeFieldLabelsNoPrefix ''InstWithInfo)

combineBody :: LL.FunBody -> LL.FunBody
combineBody = parOver Parallel.rseq LL.bodyBlocks combineBlock

combineBlock :: LL.Block -> LL.Block
combineBlock block = block'
  where
    block' =
      LL.Block
        { insts = toVectorOf (#idToInst % each % #inst) combineState',
          term = combineState' ^. #term
        }
    ((), !combineState') = runPureEff $ runState combineState combineLoop
    combineState = stateFromBlock block

combineLoop :: (State CombineState :> es) => Eff es ()
combineLoop = do
  prevId <- use @CombineState #prevId
  idToInst <- use @CombineState #idToInst
  case IntMap.lookupGT prevId idToInst of
    Just (currId, inst) -> do
      combineInst currId inst
      assign @CombineState #prevId currId
      combineLoop
    Nothing -> pure ()

combineInst :: (State CombineState :> es) => Int -> InstWithInfo -> Eff es ()
combineInst id inst = do
  st <- get
  case combineInst' id inst st of
    Just (toDelete, toInsert, inst') -> do
      assign @CombineState (#idToInst % at toDelete) Nothing
      assign @CombineState (#idToInst % at toInsert) (Just inst')
    Nothing -> pure ()

combineInst' :: Int -> InstWithInfo -> CombineState -> Maybe (Int, Int, InstWithInfo)
combineInst' id inst st = do
  -- don't move alloca around
  guard $ not $ has #_Alloca $ inst ^. #inst
  -- TODO: for now stuff without names cannot be combined
  name <- inst ^? #inst % LL.instName
  useId <- st ^. #idToUses % at id
  let useInst = st ^?! #idToInst % ix useId
  -- for now if the next instructions uses the name twice or more, we cannot combine it
  -- maybe we should have some sort of seq instruction
  case useInst ^. #inst
    ^.. LL.instOperands
    % filtered
      ( \operand ->
          Just name == (operand ^? LL.operandName)
      ) of
    _ : _ : _ -> Nothing
    _ -> pure ()
  let between = inBetween (RangeP id useId) (st ^. #idToInst)
  let substDownRes = Just (id, useId, substInstWithInfo inst useInst)
  -- very conservative for now
  -- things to consider, substituting up
  -- use alias analysis to see if we can move stuff around
  if
      | isPure inst -> substDownRes
      | allOf each isPure between && isPure useInst -> substDownRes
      | otherwise -> Nothing

isPure :: InstWithInfo -> Bool
isPure InstWithInfo {hasCall, hasMem} =
  not $ hasCall || hasMem

substInstWithInfo :: InstWithInfo -> InstWithInfo -> InstWithInfo
substInstWithInfo inst inst' =
  InstWithInfo
    { inst = substInstInst (inst ^. #inst) (inst' ^. #inst),
      hasCall = inst ^. #hasCall || inst' ^. #hasCall,
      hasMem = inst ^. #hasMem || inst' ^. #hasMem
    }

substInstInst :: LL.Inst -> LL.Inst -> LL.Inst
substInstInst inst inst' = inst' & LL.instOperands %~ substInstOperand inst

substInstOperand :: LL.Inst -> LL.Operand -> LL.Operand
substInstOperand inst operand = case (inst ^? LL.instName, operand) of
  (Just name, LL.Temp name') | name == name' -> LL.Nested inst
  (Just _name, LL.Nested inst') -> LL.Nested $ inst' & LL.instOperands %~ substInstOperand inst
  (Just _name, other) -> other
  (Nothing, other) -> other

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
    idToInst = IntMap.fromList $ toList instsWithId
    nameToInst = HashMap.toMapOf (folded % _Just % ifolded) instWithName
    instWithName =
      fmap
        ( \(id, inst) -> do
            name <- inst ^? #inst % LL.instName
            pure (name, (id, inst))
        )
        instsWithId
    insts =
      fmap
        ( \inst ->
            InstWithInfo
              { inst,
                hasCall = has #_Call inst,
                hasMem = has #_Store inst || has #_Load inst
              }
        )
        (block ^. #insts)
    instsWithId = V.zip (fromList [1 :: Int ..]) insts
    idToUses = foldl' go Empty instsWithId
      where
        go :: IntMap Int -> (Int, InstWithInfo) -> IntMap Int
        go uses (id, inst) =
          uses
            & at id
            %~ ( \useId -> case (useId, ids) of
                   (alreadyAt, []) -> alreadyAt
                   (Nothing, [id]) -> Just id
                   (Nothing, _) -> Nothing
                   (Just _, _ : _) -> Nothing
               )
          where
            names = inst ^.. #inst % LL.instOperands % LL.operandName
            ids = fmap (\name -> nameToInst ^? at name % _Just % _1) names ^.. folded % _Just
