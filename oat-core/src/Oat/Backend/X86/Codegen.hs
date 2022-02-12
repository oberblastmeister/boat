module Oat.Backend.X86.Codegen
  ( compileProg,
  )
where

import Control.Source (Source)
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Effectful.Reader.Static
import Effectful.Reader.Static.Optics
import Oat.Asm.AST (pattern (:@))
import Oat.Asm.AST qualified as Asm
import Oat.Backend.X86.Frame qualified as Frame
import Oat.Backend.X86.Munch (BackendEnv (..))
import Oat.Backend.X86.Munch qualified as Munch
import Oat.Backend.X86.RegAlloc qualified as RegAlloc
import Oat.Backend.X86.X86 (InstLab, Reg (..))
import Oat.Backend.X86.X86 qualified as X86
import Oat.Common (concatToEither)
import Oat.LL qualified as LL
import Prelude

compileProg :: '[Source LL.Name] :>> es => LL.Prog -> Eff es (Seq InstLab)
compileProg = compileDeclMap . LL.progToDeclMap

compileDeclMap :: '[Source LL.Name] :>> es => LL.DeclMap -> Eff es (Seq InstLab)
compileDeclMap declMap = do
  let env = Munch.BackendEnv {tyMap = declMap ^. #tyDecls % #map}
  funDecls <- traverse (uncurry $ compileFunDecl env) (declMap ^. #funDecls % #list)
  globalDecls <- traverse (uncurry $ compileGlobalDecl env) (declMap ^. #globalDecls % #list)
  pure $
    fold funDecls
      <> fold globalDecls

compileGlobalDecl :: '[Source LL.Name] :>> es => BackendEnv -> LL.Name -> LL.GlobalDecl -> Eff es (Seq InstLab)
compileGlobalDecl globalDecl = mempty

compileFunDecl ::
  forall es.
  Source LL.Name :> es =>
  BackendEnv ->
  LL.Name ->
  LL.FunDecl ->
  Eff es (Seq InstLab)
compileFunDecl env name funDecl = runReader env $ do
  (insts, frameState) <- Frame.runFrame $ do
    insts <- Munch.compileBody $ funDecl ^. #body
    let viewShiftedInsts = Seq.fromList (Right <$> viewShiftFrom (funDecl ^. #params)) <> insts
    RegAlloc.noReg viewShiftedInsts
  tyMap <- rview #tyMap
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
            X86.Movq :@ [Asm.Reg reg, Asm.Temp name]
          (name, Right n) ->
            X86.Movq :@ [Asm.Mem $ X86.MemStackSimple n, Asm.Temp name]
      )

prologueEpilogue :: Maybe Int -> Frame.FrameState -> (Seq X86.Inst, Seq X86.Inst)
prologueEpilogue maybeMaxCall frameState = (prologue, epilogue)
  where
    prologue =
      Seq.fromList
        [ X86.Pushq :@ [Asm.Reg Rbp],
          X86.Movq :@ [Asm.Reg Rsp, Asm.Reg Rbp]
        ]
        <> subStack
    epilogue =
      addStack
        <> Seq.fromList
          [ X86.Popq :@ [Asm.Reg Rbp],
            X86.Retq :@ []
          ]
    subStack = Seq.fromList [X86.Subq :@ [stackSizeArg, Asm.Reg Rsp]]
    addStack = Seq.fromList [X86.Addq :@ [stackSizeArg, Asm.Reg Rsp] | stackSize /= 0]
    stackSize = nextMultipleOf16 (fromIntegral maxCall + Frame.getStackSize frameState)
    stackSizeArg = Asm.Imm $ X86.Lit $ fromIntegral stackSize
    maxCall = max 0 (fromMaybe 0 maybeMaxCall - X86.wordSize * length X86.paramRegs)
    -- TODO: do we need this?
    nextMultipleOf16 :: Int -> Int
    nextMultipleOf16 n = 16 * ((n + 15) `div` 16)
