{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.Check
  ( checkProg,
    CheckError,
  )
where

import Data.HashSet qualified as HashSet
import Data.MapList qualified as MapList
import Effectful.Reader.Static
import Effectful.Reader.Static.Optics
import Effectful.State.Static.Local
import Effectful.State.Static.Local.Optics
import Effectful.Writer.Static.Local
import Oat.Common (leftToMaybe, maybeToLeft, unwrap, type (++))
import Oat.LL.AST qualified as LL
import Oat.LL.Name qualified as LL

data CheckState = CheckState
  { tempToTy :: HashMap LL.Name LL.Ty
  }

data CheckError
  = NameNotFound LL.Name
  | -- custom message, arg, expected, actual
    MismatchedTy (Maybe Text) (Maybe LL.Operand) [LL.Ty] LL.Ty
  | InterferingGlobals
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''CheckState

type StateCheckEffs =
  '[ ErrorWriter,
     State CheckState,
     Reader LL.DeclMap
   ]

type CheckEffs =
  '[ ErrorWriter,
     Reader LL.TyMap,
     Reader LL.DeclMap
   ]

type ErrorWriter = Writer (Dual [CheckError])

checkProg :: LL.Prog -> Maybe (NonEmpty CheckError)
checkProg prog = run $ do
  checkDeclMap $ prog ^. #declMap
  checkDecls $ prog ^. #decls
  where
    run =
      leftToMaybe
        . runPureEff
        . runCheck
          (prog ^. #declMap)

checkDecls :: StateCheckEffs :>> es => [LL.Decl] -> Eff es ()
checkDecls = traverse_ checkDecl

checkDeclMap :: ErrorWriter :> es => LL.DeclMap -> Eff es ()
checkDeclMap declMap = do
  let gidUnion =
        (declMap ^. #globalDecls % to MapList.keysSet)
          `HashSet.union` (declMap ^. #funDecls % to MapList.keysSet)
          `HashSet.union` (declMap ^. #externDecls % to MapList.keysSet)
  unless (HashSet.null gidUnion) $
    tellErrors [InterferingGlobals]

checkDecl :: StateCheckEffs :>> es => LL.Decl -> Eff es ()
checkDecl (LL.DeclFun _name funDecl) = traverseOf_ (#body % LL.bodyBlocks) checkBlock funDecl
checkDecl _ = pure ()

checkBlock :: StateCheckEffs :>> es => LL.Block -> Eff es ()
checkBlock block = do
  forOf_ (#insts % traversed) block $ \inst -> do
    tempToTy <- use @CheckState #tempToTy
    ty <- runReader tempToTy $ inferInst inst
    case inst ^? LL.instName of
      Nothing -> pure ()
      Just name -> assign @CheckState (#tempToTy % at name) (Just ty)
  tempToTy <- use @CheckState #tempToTy
  runReader tempToTy $ checkTerm (block ^. #term)

inferInst :: CheckEffs :>> es => LL.Inst -> Eff es LL.Ty
inferInst = \case
  LL.BinOp inst -> inferBinOp inst
  LL.Alloca inst -> inferAlloca inst
  LL.Load inst -> inferLoad inst
  LL.Store inst -> inferStore inst
  LL.Icmp inst -> inferIcmp inst
  LL.Call inst -> inferCall inst
  LL.Bitcast inst -> inferBitcast inst
  LL.Gep inst -> inferGep inst
  LL.Select inst -> inferSelect inst

checkTerm :: CheckEffs :>> es => LL.Term -> Eff es ()
checkTerm (LL.Ret LL.RetTerm {ty, arg}) = case arg of
  Nothing -> pure ()
  Just arg -> checkOperand arg ty
checkTerm (LL.Cbr LL.CbrTerm {arg}) = checkOperand arg LL.I1
checkTerm (LL.Br _) = pure ()

checkOperand :: CheckEffs :>> es => LL.Operand -> LL.Ty -> Eff es ()
checkOperand (LL.Const _) LL.I1 = pure ()
checkOperand (LL.Const _) LL.I8 = pure ()
checkOperand (LL.Const _) LL.I64 = pure ()
checkOperand arg@(LL.Const _) ty = tellErrors [MismatchedTy Nothing (Just arg) [LL.I1, LL.I8, LL.I64] ty]
checkOperand arg@(LL.Gid name) ty = do
  ty' <- rview @LL.DeclMap $ #globalDecls % #map % at name % unwrap (error "impossible") % #ty
  tyAssert (Just "gid") (Just arg) [ty'] ty
checkOperand (LL.Nested _) _ = error "There must not be any nested when checking ll"
checkOperand arg@(LL.Temp name) ty =
  rview @LL.TyMap (at name) >>= \case
    Just ty' -> tyAssert (Just "name") (Just arg) [ty] ty'
    Nothing -> tellErrors [NameNotFound name]

inferBinOp :: CheckEffs :>> es => LL.BinOpInst -> Eff es LL.Ty
inferBinOp LL.BinOpInst {ty, arg1, arg2} = do
  tyAssert (Just "binary operator") Nothing [LL.I1, LL.I8, LL.I64] ty
  checkOperand arg1 ty
  checkOperand arg2 ty
  pure ty

inferAlloca :: LL.AllocaInst -> Eff es LL.Ty
inferAlloca LL.AllocaInst {ty} = pure ty

inferLoad :: CheckEffs :>> es => LL.LoadInst -> Eff es LL.Ty
inferLoad LL.LoadInst {ty, arg} = do
  checkOperand arg (LL.TyPtr ty)
  pure ty

inferStore :: CheckEffs :>> es => LL.StoreInst -> Eff es LL.Ty
inferStore LL.StoreInst {ty1, arg1, ty2, arg2} = do
  checkOperand arg1 ty1
  checkOperand arg2 ty2
  pure LL.Void

inferIcmp :: CheckEffs :>> es => LL.IcmpInst -> Eff es LL.Ty
inferIcmp LL.IcmpInst {ty, arg1, arg2} = do
  checkOperand arg1 LL.I64
  checkOperand arg2 LL.I64
  tyAssert (Just "icmp must return bool") Nothing [LL.I1] ty
  pure LL.I1

inferCall :: CheckEffs :>> es => LL.CallInst -> Eff es LL.Ty
inferCall LL.CallInst {ty, fn, args} = do
  checkOperand fn (LL.TyFun LL.FunTy {args = fst <$> args, ret = ty})
  for_ args $ \(ty, arg) -> checkOperand arg ty
  pure ty

inferBitcast :: CheckEffs :>> es => LL.BitcastInst -> Eff es LL.Ty
inferBitcast LL.BitcastInst {from, to, arg} = do
  checkOperand arg from
  pure to

inferGep :: CheckEffs :>> es => LL.GepInst -> Eff es LL.Ty
inferGep = undefined

inferSelect :: CheckEffs :>> es => LL.SelectInst -> Eff es LL.Ty
inferSelect = undefined

tellErrors :: ErrorWriter :> es => [CheckError] -> Eff es ()
tellErrors = tell . Dual

tyAssert :: Writer (Dual [CheckError]) :> es => Maybe Text -> Maybe LL.Operand -> [LL.Ty] -> LL.Ty -> Eff es ()
tyAssert mess arg expectedTys actualTy =
  unless (actualTy `elem` expectedTys) $
    tellErrors [MismatchedTy mess arg expectedTys actualTy]

defCheckState :: CheckState
defCheckState = CheckState {tempToTy = mempty}

runCheck :: LL.DeclMap -> Eff (StateCheckEffs ++ es) a -> Eff es (Either (NonEmpty CheckError) a)
runCheck declMap m = do
  (a, Dual es) <- run m
  pure $ maybeToLeft a $ nonEmpty es
  where
    run =
      runWriter @(Dual [CheckError])
        >>> evalState @CheckState defCheckState
        >>> runReader @LL.DeclMap declMap