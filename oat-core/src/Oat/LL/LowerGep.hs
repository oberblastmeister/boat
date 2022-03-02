module Oat.LL.LowerGep
  ( lowerAll,
    lower,
    lowerFunDecl,
  )
where

import Data.Vector qualified as V
import Effectful.Reader.Static (Reader)
import Oat.LL.Ast qualified as LL
import Oat.LL.Name qualified as LL
import Oat.Utils.Impossible (impossible)
import Optics.Operators.Unsafe ((^?!))

lowerFunDecl :: '[Reader LL.DeclMap, LL.NameSource] :>> es => LL.FunDecl -> Eff es LL.FunDecl
lowerFunDecl = traverseOf (#body % LL.bodyBlocks % #insts) lowerAll

lowerAll :: (Traversable t, Reader LL.DeclMap :> es, LL.NameSource :> es) => t LL.Inst -> Eff es (Vec LL.Inst)
lowerAll insts = do
  insts <- for insts $ \case
    LL.Gep inst -> lower inst
    inst -> pure [inst]
  pure $ fromList $ concat insts

lower :: '[Reader LL.DeclMap, LL.NameSource] :>> es => LL.GepInst -> Eff es [LL.Inst]
lower LL.GepInst {name, ty = tyPtr, arg, args} = do
  let tyInner = case tyPtr of
        LL.TyPtr ty -> ty
        _ -> impossible

  (_, insts, _) <-
    foldM
      ( \(ty, insts, prevArg) (_argTy, arg) -> do
          case (ty, arg) of
            (LL.TyStruct tys, LL.Const i) -> do
              offset <- V.take i tys & traverse LL.tySize <&> sum
              name <- LL.freshName
              let inst =
                    LL.BinOp
                      LL.BinOpInst
                        { name,
                          op = LL.Add,
                          ty = tyPtr,
                          arg1 = prevArg,
                          arg2 = LL.Const offset
                        }
              pure (tys ^?! ix i, inst : insts, LL.Temp name)
            (LL.TyStruct _, _) -> impossible
            (LL.TyArray _arrSize ty', arg) -> do
              name1 <- LL.freshName
              name2 <- LL.freshName
              ty'Size <- LL.tySize ty'
              let mulInst =
                    LL.BinOp
                      LL.BinOpInst
                        { name = name1,
                          op = LL.Mul,
                          ty = LL.I64,
                          arg1 = arg,
                          arg2 = LL.Const ty'Size
                        }
              let inst =
                    LL.BinOp
                      LL.BinOpInst
                        { name = name2,
                          op = LL.Add,
                          ty = tyPtr,
                          arg1 = prevArg,
                          arg2 = LL.Temp name1
                        }
              pure (ty', inst : mulInst : insts, LL.Temp name2)
            _ -> impossible
      )
      (tyInner, [], arg)
      args
  let insts' = case insts of
        inst : insts -> (inst & LL.instName .~ name) : insts
        [] -> impossible
  pure (reverse insts')
