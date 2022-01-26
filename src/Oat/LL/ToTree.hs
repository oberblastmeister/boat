module Oat.LL.ToTree where

import qualified Data.HashMap.Lazy as HashMap
import Oat.Common (unwrap)
import Oat.LL.AST
import Oat.LL.Name (Name)
import Optics
import Optics.State.Operators
import Prelude hiding (Const)

type TempMap = HashMap Name (Inst 'Flat, Maybe (Inst 'Tree))

type MonadToTree m = MonadState TempMap m

funBodyToTree :: FunBody 'Flat -> FunBody 'Tree
funBodyToTree body = evalState (funBodyToTreeWith body) tempMap
  where
    tempMap = getTempMap body

funBodyToTreeWith :: MonadState TempMap m => FunBody 'Flat -> m (FunBody 'Tree)
funBodyToTreeWith FunBody {entry, labeled} = do
  entry <- blockToTree entry
  labeled <- traverseOf (traversed % #block) blockToTree labeled
  pure FunBody {entry, labeled}

blockToTree :: MonadToTree m => Block 'Flat -> m (Block 'Tree)
blockToTree Block {inst, term} = do
  inst <- traverse toTree inst
  term <- toTree term
  pure Block {inst, term}

toTree :: MonadToTree m => Inst' s 'Flat -> m (Inst' s 'Tree)
toTree inst = do
  forOf instOperands inst $ \case
    Null -> pure Null
    Const i -> pure $ Const i
    Gid name -> pure $ Gid name
    Temp name -> do
      (instAt, maybeRest) <- use $ at name % unwrap (error "")
      case maybeRest of
        Just rest -> pure $ TempTree name (Just rest)
        Nothing -> do
          rest <- toTreeDeep instAt
          at name % unwrap (error "") % _2 .= rest
          pure $ TempTree name rest

-- see if the operations have side effects before converting them
toTreeDeep :: MonadToTree m => Inst' s 'Flat -> m (Maybe (Inst' s 'Tree))
toTreeDeep = \case
  Alloca _ -> pure Nothing
  Store _ -> pure Nothing
  Call _ -> pure Nothing
  inst -> Just <$> toTree inst

getTempMap :: FunBody 'Flat -> TempMap
getTempMap body = tempMapEntry <> tempMapLabeled
  where
    tempMapEntry = getTempMapInsts (body ^. #entry % #inst) <> tempMapLabeled
    tempMapLabeled =
      foldMap' id $
        ( body
            ^.. #labeled
            % folded
            % #block
            % #inst
        )
          <&> getTempMapInsts

getTempMapInsts :: [Inst 'Flat] -> TempMap
getTempMapInsts insts =
  HashMap.fromList $
    ( insts & traversed
        %~ (\inst -> inst ^? instName & _Just %~ (,(inst, Nothing)))
    )
      ^.. folded
      % _Just
