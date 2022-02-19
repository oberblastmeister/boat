module Oat.Backend.X86.Codegen
  ( compileProg,
  )
where

import Control.Source (Source)
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Effectful.Reader.Static
import Effectful.Reader.Static.Optics
import Oat.Backend.X86.Frame qualified as Frame
import Oat.Backend.X86.Munch qualified as Munch
import Oat.Backend.X86.RegAlloc qualified as RegAlloc
import Oat.Backend.X86.X86 (InstLab, Reg (..), pattern (:@))
import Oat.Backend.X86.X86 qualified as X86
import Oat.Common (concatToEither)
import Oat.LL qualified as LL
import Prelude

compileProg :: '[Source LL.Name] :>> es => LL.Prog -> Eff es (Seq InstLab)
compileProg LL.Prog {declMap} = compileDeclMap declMap

compileDeclMap :: '[Source LL.Name] :>> es => LL.DeclMap -> Eff es (Seq InstLab)
compileDeclMap declMap = runReader declMap $ do
  funDecls <- traverse (uncurry compileFunDecl) (declMap ^. #funDecls % #list)
  globalDecls <- traverse (uncurry compileGlobalDecl) (declMap ^. #globalDecls % #list)
  pure $
    fold funDecls
      <> fold globalDecls

compileGlobalDecl :: '[Reader LL.DeclMap, Source LL.Name] :>> es => LL.Name -> LL.GlobalDecl -> Eff es (Seq InstLab)
compileGlobalDecl = mempty

compileFunDecl ::
  forall es.
  '[Reader LL.DeclMap, Source LL.Name] :>> es =>
  LL.Name ->
  LL.FunDecl ->
  Eff es (Seq InstLab)
compileFunDecl name funDecl = do
  (insts, frameState) <- Frame.runFrame $ do
    insts <- Munch.compileBody $ funDecl ^. #body
    let viewShiftedInsts = Seq.fromList (Right <$> viewShiftFrom (funDecl ^. #params)) <> insts
    RegAlloc.noReg viewShiftedInsts
  tyMap <- rview @LL.DeclMap $ #tyDecls % #map
  let maxCall = LL.maxCallSize tyMap (funDecl ^. #body)
      (prologue, epilogue) = prologueEpilogue maxCall frameState
  pure $ Left (name, True) :< (fmap Right prologue <> insts <> fmap Right epilogue)

viewShiftFrom :: [LL.Name] -> [X86.Inst]
viewShiftFrom args =
  -- it starts from two because we need to skip the implicitly pushed %rip from callq and also skip the pushed %rbp
  concatToEither X86.paramRegs [i * 8 | i <- [2 :: Int64 ..]]
    & zip args
    & fmap
      ( \case
          (name, Left reg) ->
            X86.Movq :@ [X86.OReg reg, X86.OTemp name]
          (name, Right n) ->
            X86.Movq :@ [X86.OMem $ X86.MemStackSimple n, X86.OTemp name]
      )

prologueEpilogue :: Maybe Int -> Frame.FrameState -> (Seq X86.Inst, Seq X86.Inst)
prologueEpilogue maybeMaxCall frameState = (prologue, epilogue)
  where
    prologue =
      Seq.fromList
        [ X86.Pushq :@ [X86.OReg Rbp],
          X86.Movq :@ [X86.OReg Rsp, X86.OReg Rbp]
        ]
        <> subStack
    epilogue =
      addStack
        <> Seq.fromList
          [ X86.Popq :@ [X86.OReg Rbp],
            X86.Retq :@ []
          ]
    subStack = Seq.fromList [X86.Subq :@ [stackSizeArg, X86.OReg Rsp]]
    addStack = Seq.fromList [X86.Addq :@ [stackSizeArg, X86.OReg Rsp] | stackSize /= 0]
    -- stackSize = nextMultipleOf16 (fromIntegral maxCall + Frame.getStackSize frameState)
    stackSize = fromIntegral maxCall + Frame.getStackSize frameState
    stackSizeArg = X86.OImm $ X86.Lit $ fromIntegral stackSize
    maxCall = max 0 (fromMaybe 0 maybeMaxCall - X86.wordSize * length X86.paramRegs)
    -- TODO: do we need this?
    nextMultipleOf16 :: Int -> Int
    nextMultipleOf16 n = 16 * ((n + 15) `div` 16)
