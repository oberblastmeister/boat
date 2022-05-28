module Oat.Backend.X86.Codegen
  ( compileModule,
  )
where

import Acc (Acc, cons)
import Data.Int (Int64)
import Data.Vector qualified as V
import Effectful.Reader.Static
import Oat.Backend.X86.Frame qualified as Frame
import Oat.Backend.X86.Munch qualified as Munch
import Oat.Backend.X86.RegAlloc qualified as RegAlloc
import Oat.Backend.X86.X86 (InstLab, Reg (..), pattern (:@))
import Oat.Backend.X86.X86 qualified as X86
import Oat.LL qualified as LL
import Oat.LL.LowerGep qualified as LL.LowerGep
import Oat.Utils.Misc (concatToEither)
import Oat.Utils.Source (Source)
import Prelude

compileModule :: '[Source LL.Name] :>> es => LL.Module -> Eff es (Acc InstLab)
compileModule LL.Module {declMap} = compileDeclMap declMap

compileDeclMap :: '[Source LL.Name] :>> es => LL.DeclMap -> Eff es (Acc InstLab)
compileDeclMap declMap = runReader declMap $ do
  funDecls <- traverse (uncurry compileFunDecl) (declMap ^. #funDecls % #list)
  globalDecls <- traverse (uncurry compileGlobalDecl) (declMap ^. #globalDecls % #list)
  pure $
    fold funDecls
      <> fold globalDecls

compileGlobalDecl :: '[Reader LL.DeclMap, Source LL.Name] :>> es => LL.Name -> LL.GlobalDecl -> Eff es (Acc InstLab)
compileGlobalDecl = mempty

compileFunDecl ::
  forall es.
  '[Reader LL.DeclMap, Source LL.Name] :>> es =>
  LL.Name ->
  LL.FunDecl ->
  Eff es (Acc InstLab)
compileFunDecl name funDecl = do
  (insts, frameState) <- Frame.runFrame $ do
    funDecl <- LL.LowerGep.lowerFunDecl funDecl
    insts <- Munch.compileBody $ funDecl ^. #body
    let viewShiftedInsts =
          ( fromList @(Acc _) $
              toList @(Vec _)
                ( Right
                    <$> viewShiftFrom (funDecl ^. #params)
                )
          )
            <> insts
    RegAlloc.noReg viewShiftedInsts
  maxCall <- LL.maxCallSize $ funDecl ^. #body
  let (prologue, epilogue) = prologueEpilogue maxCall frameState
  pure $ Left (name, True) `Acc.cons` (fmap Right prologue <> insts <> fmap Right epilogue)

viewShiftFrom :: Vec LL.Name -> Vec X86.Inst
viewShiftFrom args =
  -- it starts from two because we need to skip the implicitly pushed %rip from callq and also skip the pushed %rbp
  (fromList (concatToEither X86.paramRegs [i * 8 | i <- [2 :: Int64 ..]]))
    & V.zip args
    & V.map
      ( \case
          (name, Left reg) ->
            X86.Movq :@ [X86.OReg reg, X86.OTemp name]
          (name, Right n) ->
            X86.Movq :@ [X86.OMem $ X86.MemStackSimple n, X86.OTemp name]
      )
{-# INLINE viewShiftFrom #-}

prologueEpilogue :: Maybe Int -> Frame.FrameState -> (Acc X86.Inst, Acc X86.Inst)
prologueEpilogue maybeMaxCall frameState = (prologue, epilogue)
  where
    prologue =
      fromList
        [ X86.Pushq :@ [X86.OReg Rbp],
          X86.Movq :@ [X86.OReg Rsp, X86.OReg Rbp]
        ]
        <> subStack
    epilogue =
      addStack
        <> fromList
          [ X86.Popq :@ [X86.OReg Rbp],
            X86.Retq :@ []
          ]
    subStack = fromList [X86.Subq :@ [stackSizeArg, X86.OReg Rsp]]
    addStack = fromList [X86.Addq :@ [stackSizeArg, X86.OReg Rsp] | stackSize /= 0]
    -- the stack must be aligned to 16 bytes as mandated by the System-V calling convention
    stackSize = nextMultipleOf16 (maxCall + Frame.getStackSize frameState)
    stackSizeArg = X86.OImm $ X86.Lit $ fromIntegral stackSize
    maxCall = max 0 (fromMaybe 0 maybeMaxCall - X86.wordSize * length X86.paramRegs)
    nextMultipleOf16 :: Int -> Int
    nextMultipleOf16 n = 16 * ((n + 15) `div` 16)
