{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.LL.Check
  ( checkProg,
    CheckError,
  )
where

import Data.HashSet qualified as HashSet
import Data.MapList qualified as MapList
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader, ask)
import Effectful.Reader.Static qualified as Reader
import Effectful.Reader.Static.Optics (rview)
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import Effectful.State.Static.Local.Optics (assign, use)
import Oat.Error (CompileFail)
import Oat.LL.AST qualified as LL
import Oat.LL.Name qualified as LL
import Oat.Reporter (Reporter, report)
import Oat.Reporter qualified as Reporter

data CheckState = CheckState
  { tempToTy :: !LL.TyMap,
    didFail :: !Bool
  }

data CheckError
  = NameNotFound LL.Name
  | -- custom message, arg, expected, actual
    MismatchedTy (Maybe Text) (Maybe LL.Operand) [LL.Ty] LL.Ty
  | InterferingGlobals
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''CheckState

type CheckEffs =
  '[ Reporter [CheckError],
     Reader LL.TyMap,
     Reader LL.DeclMap,
     State LL.TyMap
   ]

checkProg :: '[Reporter [CheckError], Error CompileFail] :>> es => LL.Prog -> Eff es ()
checkProg prog = do
  checkDeclMap $ prog ^. #declMap
  State.evalState @LL.TyMap mempty
    . Reader.runReader @LL.DeclMap (prog ^. #declMap)
    . checkDecls
    $ prog ^. #decls
  Reporter.maybeFail @[CheckError]

checkDecls :: '[Reporter [CheckError], Reader LL.DeclMap, State LL.TyMap] :>> es => [LL.Decl] -> Eff es ()
checkDecls = traverse_ checkDecl

checkDeclMap :: Reporter [CheckError] :> es => LL.DeclMap -> Eff es ()
checkDeclMap declMap = do
  let gidUnion =
        (declMap ^. #globalDecls % to MapList.keysSet)
          `HashSet.intersection` (declMap ^. #funDecls % to MapList.keysSet)
          `HashSet.intersection` (declMap ^. #externDecls % to MapList.keysSet)
  unless (HashSet.null gidUnion) $
    report [InterferingGlobals]

checkDecl :: '[Reporter [CheckError], Reader LL.DeclMap, State LL.TyMap] :>> es => LL.Decl -> Eff es ()
checkDecl (LL.DeclFun _name funDecl) = State.evalState @CheckState defCheckState $ do
  for_ (LL.funDeclTyParams funDecl) $ \(param, arg) ->
    assign @CheckState (#tempToTy % at param) (Just arg)
  traverseOf_ (#body % LL.bodyBlocks) checkBlock funDecl
checkDecl _ = pure ()

checkBlock ::
  '[ Reporter [CheckError],
     Reader LL.DeclMap,
     State CheckState,
     State LL.TyMap
   ]
    :>> es =>
  LL.Block ->
  Eff es ()
checkBlock block = do
  forOf_ (#insts % traversed) block $ \inst -> do
    tempToTy <- use @CheckState #tempToTy
    ty <- Reader.runReader tempToTy $ inferInst inst
    case inst ^? LL.instName of
      Nothing -> pure ()
      Just name -> assign @CheckState (#tempToTy % at name) (Just ty)
  tempToTy <- use @CheckState #tempToTy
  Reader.runReader tempToTy $ checkTerm (block ^. #term)

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
checkOperand arg@(LL.Const _) ty = report [MismatchedTy Nothing (Just arg) [LL.I1, LL.I8, LL.I64] ty]
checkOperand arg@(LL.Gid name) ty = do
  declMap <- ask @LL.DeclMap
  case headOf (LL.declMapTyAt name) declMap of
    Nothing ->
      use @LL.TyMap (at name)
        >>= ( \case
                Nothing -> assign @LL.TyMap (at name) (Just ty)
                Just ty' -> tyAssert (Just "saved type") (Just arg) [ty'] ty
            )
    Just ty' -> tyAssert (Just "global decls") (Just arg) [ty'] ty
checkOperand (LL.Nested _) _ = error "There must not be any nested when checking ll"
checkOperand arg@(LL.Temp name) ty =
  rview @LL.TyMap (at name) >>= \case
    Just ty' -> tyAssert (Just "name") (Just arg) [ty] ty'
    Nothing -> report [NameNotFound name]

inferBinOp :: CheckEffs :>> es => LL.BinOpInst -> Eff es LL.Ty
inferBinOp LL.BinOpInst {ty, arg1, arg2} = do
  tyAssert (Just "binary operator") Nothing [LL.I1, LL.I8, LL.I64] ty
  checkOperand arg1 ty
  checkOperand arg2 ty
  pure ty

inferAlloca :: LL.AllocaInst -> Eff es LL.Ty
inferAlloca LL.AllocaInst {ty} = pure $ LL.TyPtr ty

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

tyAssert :: Reporter [CheckError] :> es => Maybe Text -> Maybe LL.Operand -> [LL.Ty] -> LL.Ty -> Eff es ()
tyAssert mess arg expectedTys actualTy =
  unless (actualTy `elem` expectedTys) $
    report [MismatchedTy mess arg expectedTys actualTy]

defCheckState :: CheckState
defCheckState = CheckState {tempToTy = mempty, didFail = False}