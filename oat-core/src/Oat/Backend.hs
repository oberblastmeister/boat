{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Backend where

import Control.Exception (assert)
import Control.Monad qualified as Monad
import Data.ASCII (ASCII)
import Data.List ((!!))
import Oat.Alloc (Loc (..), Operand (..))
import Oat.Alloc qualified as Alloc
import Oat.Common (internalError, unreachable)
import Oat.Fold (paraOf)
import Oat.LL qualified as LL
import Oat.LL.AST (Named (Do), pattern (:=))
import Oat.LL.Name (Name)
-- import Oat.X86.AST (Operand ((:$), (:$$), (:%)), (@@))
import Oat.X86.AST qualified as X86
import Optics hiding ((:>))
import Optics.State.Operators
import Prelude hiding (Const)

-- -- emit instructions using conduit?
-- -- probably for now emit instructions using snoc list
-- -- is it better to use lazy state because we are building up a lazy snoclist?
-- -- the instructions that come in should probably be a Seq
-- data BackendState = BackendState
--   { insStream :: X86Stream,
--     tyDecls :: HashMap (ASCII ByteString) LL.Ty
--   }

-- type BackendM = State BackendState

-- type MonadBackend = (MonadState BackendState)

-- data X86Elt
--   = I X86.inst
--   | L !(ASCII ByteString) !Bool -- label, whether global or not

-- type X86Stream = [X86Elt]

-- makeFieldLabelsNoPrefix ''BackendState

-- runBackend :: BackendM a -> BackendState -> a
-- runBackend = evalState

-- inst :: [X86.inst] -> X86Stream
-- inst = fmap I

-- emit :: MonadBackend m => X86Stream -> m ()
-- emit inst = #insStream %= (++> inst)

-- emitIns :: MonadBackend m => [X86.inst] -> m ()
-- emitIns = emit . inst

-- emitMov :: MonadBackend m => X86.Operand -> X86.Operand -> m ()
-- emitMov src dst = case (src, dst) of
--   (X86.Imm (X86.Lab _), _) -> error "immediate label not supported yet"
--   (X86.Imm _, X86.Imm _) -> error "malformed mov"
--   (X86.Imm _, _) -> mov
--   (_, X86.Imm _) -> mov
--   (X86.Reg _, _) -> mov
--   (_, X86.Reg _) -> mov
--   (_, _) -> emitIns $ [X86.Movq @@ [src, (:%) X86.Rax]] :> X86.Movq @@ [(:%) X86.Rax, dst]
--   where
--     mov = emitIns [X86.Movq @@ [src, dst]]

-- tySize' :: HashMap (ASCII ByteString) LL.Ty -> LL.Ty -> Int
-- tySize' tyDecls =
--   paraOf (LL.plateTy tyDecls) go
--   where
--     go ty rs =
--       case ty of
--         LL.Void -> 0
--         LL.I8 -> 0
--         LL.I1 -> 8
--         LL.I64 -> 8
--         LL.TyFun _ -> 0
--         LL.TyNamed _ -> size
--         LL.TyPtr _ -> 8
--         LL.TyArray n _ -> n * size
--         LL.TyStruct _ -> size
--       where
--         size = sum rs

-- tySize :: MonadBackend m => LL.Ty -> m Int
-- tySize ty = do
--   tyDecls <- use #tyDecls
--   pure $ tySize' tyDecls ty

-- lookupTy :: MonadBackend m => ASCII ByteString -> m LL.Ty
-- lookupTy name = do
--   mp <- use #tyDecls
--   pure $ LL.lookupTy name mp

-- compileOperand :: Alloc.Operand -> X86.Operand
-- compileOperand Null = (:$) 0
-- compileOperand (Const i) = (:$) i
-- compileOperand (Gid l) = (:$$) l
-- compileOperand (Loc loc) = compileLoc loc

-- compileLoc :: Alloc.Loc -> X86.Operand
-- compileLoc (LReg r) = (:%) r
-- compileLoc (LStack i) = X86.Ind3 (X86.Lit (fromIntegral i * 8)) X86.Rbp
-- compileLoc (LLab l) = X86.Ind1 (X86.Lab l)

-- compileAlloc :: MonadBackend m => [Alloc.inst] -> m ()
-- compileAlloc insns = for_ insns $ \inst -> do
--   case inst of
--     Alloc.ILab (LLab l) -> emit [L l False]
--     Alloc.ILab _ -> error "Malformed ILab"
--     Alloc.PMove smoves -> compilePMove smoves
--     Alloc.Icmp inst -> compileIcmp inst
--     Alloc.BinOp inst -> compileBinOp inst
--     Alloc.Alloca Alloc.AllocaIns {loc, ty} -> do
--       sz <- tySize ty
--       emitIns $
--         [X86.Subq @@ [(:$) $ fromIntegral sz, (:%) X86.Rsp]]
--           :> X86.Movq @@ [(:%) X86.Rsp, compileLoc loc]
--     Alloc.Load Alloc.LoadIns {loc, arg = (Loc (LReg r))} ->
--       emitIns [X86.Movq @@ [(:%) r, compileLoc loc]]
--     Alloc.Load Alloc.LoadIns {loc, arg} -> do
--       emitMov (compileOperand arg) ((:%) X86.Rax)
--       emitIns [X86.Movq @@ [(:%) X86.Rax, compileLoc loc]]
--     Alloc.Store Alloc.StoreIns {arg1 = (Loc (LReg r)), arg2} ->
--       emitIns [X86.Movq @@ [(:%) r, compileOperand arg2]]
--     Alloc.Store Alloc.StoreIns {arg1, arg2} ->
--       emitIns $
--         [X86.Movq @@ [compileOperand arg1, (:%) X86.Rax]]
--           :> X86.Movq @@ [(:%) X86.Rax, compileOperand arg2]
--     Alloc.Call inst -> compileCall inst
--     Alloc.Bitcast Alloc.BitcastIns {loc, arg} -> do
--       emitMov (compileOperand arg) ((:%) X86.Rax)
--       emitIns [X86.Movq @@ [(:%) X86.Rax, compileOperand (Loc loc)]]
--     Alloc.Gep inst -> compileGep inst
--     Alloc.Ret Alloc.RetIns {arg = Just arg} -> do
--       emitMov (compileOperand arg) ((:%) X86.Rax)
--       emit retIns
--     Alloc.Ret Alloc.RetIns {arg = Nothing} -> do
--       emit retIns
--     Alloc.Br arg@(LLab _) ->
--       emitIns [X86.Jmp @@ [compileOperand $ Loc arg]]
--     Alloc.Br _ -> error "malformed br instruction"
--     Alloc.Cbr inst -> compileCbr inst

-- compileCall :: MonadBackend m => Alloc.CallIns -> m ()
-- compileCall Alloc.CallIns {loc, ty, fn, args} = do
--   for_ (reverse $ zip [1 :: Int ..] args) $ \(i, (_, arg)) -> case X86.argReg i of
--     Nothing -> emitIns [X86.Pushq @@ [compileOperand arg]]
--     Just r -> emitMov (compileOperand arg) ((:%) r)
--   let nStack = length args - 6
--   emitIns $
--     [X86.Callq @@ [compileOperand fn]]
--       ++> [X86.Addq @@ [(:$) $ fromIntegral nStack, (:%) X86.Rsp] | nStack > 0]
--       ++> case loc of
--         Nothing -> []
--         Just loc' -> [X86.Movq @@ [(:%) X86.Rax, compileLoc loc'] | ty /= LL.Void]

-- compileGep :: MonadBackend m => Alloc.GepIns -> m ()
-- compileGep Alloc.GepIns {loc, ty, arg, args} = do
--   let !_ = assert (arg == Const 0) ()
--       !at = case ty of
--         LL.TyPtr ty -> ty
--         _ -> internalError "gep of non-pointer"
--   emitMov (compileOperand arg) ((:%) X86.Rax)
--   Monad.foldM_
--     ( \at arg -> do
--         case (at, arg) of
--           (LL.TyNamed name, _) -> lookupTy name
--           (LL.TyStruct tys, Alloc.Const i) -> do
--             offset <- structOffset (fromIntegral i) tys
--             emitIns [X86.Addq @@ [(:$) $ fromIntegral offset, (:%) X86.Rax]]
--             pure (tys !! fromIntegral i)
--           (LL.TyArray _ ty, Alloc.Const 0) -> pure ty
--           (LL.TyArray _ ty, _) -> do
--             emitMov ((:%) X86.Rax) ((:%) X86.Rcx)
--             emitMov (compileOperand arg) ((:%) X86.Rax)
--             size <- tySize ty
--             emitIns $
--               [X86.Imulq @@ [(:$) $ fromIntegral size]]
--                 :> X86.Addq @@ [(:%) X86.Rcx, (:%) X86.Rax]

--             pure ty
--           _ -> internalError "malformed gep instruction"
--     )
--     (LL.TyArray 0 at)
--     args
--   emitMov ((:%) X86.Rax) (compileLoc loc)

-- structOffset :: MonadBackend m => Int -> [LL.Ty] -> m Int
-- structOffset i tys =
--   take i tys
--     & Monad.foldM
--       ( \offset ty -> do
--           size <- tySize ty
--           pure $! offset + size
--       )
--       0

-- compileIcmp :: MonadBackend m => Alloc.IcmpIns -> m ()
-- compileIcmp Alloc.IcmpIns {loc, op, arg1, arg2}
--   | Loc (LReg r) <- arg1 =
--     emitIns $
--       [X86.Cmpq @@ [compileOperand arg2, (:%) r]]
--         :> mapCmpOp op @@ [compileLoc loc]
--         :> X86.Andq @@ [(:$) 1, compileLoc loc]
--   | otherwise = do
--     emitMov (compileOperand arg1) ((:%) X86.Rax)
--     emitIns $
--       [X86.Cmpq @@ [compileOperand arg2, (:%) X86.Rax]]
--         :> mapCmpOp op @@ [compileLoc loc]
--         :> X86.Andq @@ [(:$) 1, compileLoc loc]

-- compileBinOp :: MonadBackend m => Alloc.BinOpIns -> m ()
-- compileBinOp Alloc.BinOpIns {loc, op, arg1, arg2}
--   | LL.Mul <- op =
--     emitIns $
--       [X86.Movq @@ [compileOperand arg2, (:%) X86.Rax]]
--         :> X86.Imulq @@ [compileOperand arg1]
--         :> X86.Movq @@ [(:%) X86.Rax, compileLoc loc]
--   | (LReg r) <- loc,
--     Loc (LReg r') <- arg2,
--     r == r' =
--     emitIns [mapBinOp op @@ [compileOperand arg1, (:%) r]]
--   | Loc (LReg r) <- arg2 =
--     emitIns $
--       [mapBinOp op @@ [compileOperand arg1, (:%) r]]
--         :> X86.Movq @@ [(:%) r, compileLoc loc]
--   | otherwise =
--     emitIns $
--       [X86.Movq @@ [compileOperand arg2, (:%) X86.Rax]]
--         :> mapBinOp op @@ [compileOperand arg1, (:%) X86.Rax]
--         :> X86.Movq @@ [(:%) X86.Rax, compileLoc loc]

-- compilePMove :: MonadBackend m => [Alloc.SMove] -> m ()
-- compilePMove = mapM_ compileSMove

-- compileSMove :: MonadBackend m => Alloc.SMove -> m ()
-- compileSMove Alloc.SMove {loc, arg} = emitMov (compileOperand arg) (compileLoc loc)

-- mapBinOp :: LL.BinOp -> X86.OpCode
-- mapBinOp = \case
--   LL.Add -> X86.Addq
--   LL.Sub -> X86.Subq
--   LL.Mul -> X86.Imulq
--   LL.Shl -> X86.Shlq
--   LL.Lshr -> X86.Shrq
--   LL.Ashr -> X86.Sarq
--   LL.And -> X86.Andq
--   LL.Or -> X86.Orq
--   LL.Xor -> X86.Xorq

-- mapCmpOp :: LL.CmpOp -> X86.OpCode
-- mapCmpOp = \case
--   LL.Eq -> X86.Set X86.Eq
--   LL.Neq -> X86.Set X86.Neq
--   LL.Slt -> X86.Set X86.Lt
--   LL.Sle -> X86.Set X86.Le
--   LL.Sgt -> X86.Set X86.Gt
--   LL.Sge -> X86.Set X86.Ge

-- compileCbr :: MonadBackend m => Alloc.CbrIns -> m ()
-- compileCbr Alloc.CbrIns {arg = Const i, loc1 = (LLab l1), loc2 = (LLab l2)} =
--   if i == 0
--     then emitIns [X86.Jmp @@ [(:$$) l1]]
--     else emitIns [X86.Jmp @@ [(:$$) l2]]
-- compileCbr Alloc.CbrIns {arg, loc1 = (LLab l1), loc2 = (LLab l2)} =
--   emitIns $
--     [X86.Cmpq @@ [(:$) 0, compileOperand arg]]
--       :> X86.J X86.Neq @@ [(:$$) l1]
--       :> X86.Jmp @@ [(:$$) l2]
-- compileCbr _ = error "malformed cbr instruction"

-- retIns :: X86Stream
-- retIns =
--   inst $
--     [X86.Movq @@ [(:%) X86.Rbp, (:%) X86.Rsp]]
--       :> X86.Popq @@ [(:%) X86.Rbp]
--       :> X86.Retq @@ []

-- type GetLoc = Name -> Loc

-- type Mangler = Name -> Name

-- funBodyToAlloc :: GetLoc -> Mangler -> LL.FunBody -> Alloc.FunBody
-- funBodyToAlloc getLoc mangle LL.FunBody {entry, labeled} =
--   blockToAlloc getLoc mangle entry ++ concatMap (labBlockToAlloc getLoc mangle) labeled

-- labBlockToAlloc :: GetLoc -> Mangler -> LL.LabBlock -> Alloc.FunBody
-- labBlockToAlloc getLoc mangle LL.LabBlock {lab, block} =
--   Alloc.ILab (LLab $ mangle lab) : blockToAlloc getLoc mangle block

-- blockToAlloc :: GetLoc -> Mangler -> LL.Block -> Alloc.FunBody
-- blockToAlloc getLoc mangle LL.Block {inst, terminator} =
--   map (insToAlloc getLoc mangle) inst ++ [termToAlloc getLoc mangle terminator]

-- operandToAlloc :: GetLoc -> Mangler -> LL.Operand -> Alloc.Operand
-- operandToAlloc getLoc mangle = \case
--   LL.Null -> Alloc.Null
--   LL.Const i -> Alloc.Const i
--   LL.Gid x -> Alloc.Gid $ mangle x
--   LL.Temp x -> Alloc.Loc $ getLoc x

-- insToAlloc :: GetLoc -> Mangler -> LL.Named LL.inst -> Alloc.inst
-- insToAlloc getLoc mangle = \case
--   x := LL.BinOp LL.BinOpIns {op, ty, arg1, arg2} ->
--     Alloc.BinOp
--       Alloc.BinOpIns
--         { loc = getLoc x,
--           op,
--           ty,
--           arg1 = mo arg1,
--           arg2 = mo arg2
--         }
--   Do (LL.BinOp _) -> unreachable
--   x := LL.Alloca LL.AllocaIns {ty} ->
--     Alloc.Alloca Alloc.AllocaIns {loc = getLoc x, ty}
--   Do (LL.Alloca _) -> unreachable
--   x := LL.Load LL.LoadIns {ty, arg} ->
--     Alloc.Load Alloc.LoadIns {loc = getLoc x, ty, arg = mo arg}
--   Do (LL.Load _) -> unreachable
--   Do (LL.Store LL.StoreIns {ty, arg1, arg2}) ->
--     Alloc.Store Alloc.StoreIns {ty, arg1 = mo arg1, arg2 = mo arg2}
--   _ := (LL.Store _) -> unreachable
--   x := LL.Icmp LL.IcmpIns {op, ty, arg1, arg2} ->
--     Alloc.Icmp
--       Alloc.IcmpIns
--         { loc = getLoc x,
--           op,
--           ty,
--           arg1 = mo arg1,
--           arg2 = mo arg2
--         }
--   Do (LL.Icmp _) -> unreachable
--   x := LL.Call LL.CallIns {ty, fn, args} ->
--     Alloc.Call
--       Alloc.CallIns
--         { loc = Just $ getLoc x,
--           ty,
--           fn = mo fn,
--           args = args & mapped % _2 %~ mo
--         }
--   Do (LL.Call LL.CallIns {ty, fn, args}) ->
--     Alloc.Call
--       Alloc.CallIns
--         { loc = Nothing,
--           ty,
--           fn = mo fn,
--           args = args & mapped % _2 %~ mo
--         }
--   x := LL.Bitcast LL.BitcastIns {from, arg, to} ->
--     Alloc.Bitcast Alloc.BitcastIns {loc = getLoc x, from, arg = mo arg, to}
--   Do (LL.Bitcast _) -> unreachable
--   x := LL.Gep LL.GepIns {ty, arg, args} ->
--     Alloc.Gep Alloc.GepIns {loc = getLoc x, ty, arg = mo arg, args = args <&> mo}
--   where
--     mo = operandToAlloc getLoc mangle

-- termToAlloc :: GetLoc -> Mangler -> LL.Terminator -> Alloc.inst
-- termToAlloc getLoc mangle = \case
--   LL.Ret LL.RetTerm {ty, arg} -> Alloc.Ret Alloc.RetIns {ty, arg = mo <$> arg}
--   LL.Br name -> Alloc.Br $ getLoc name
--   LL.Cbr LL.CbrTerm {arg, lab1, lab2} ->
--     Alloc.Cbr Alloc.CbrIns {arg = mo arg, loc1 = getLoc lab1, loc2 = getLoc lab2}
--   where
--     mo = operandToAlloc getLoc mangle

-- data Layout = Layout
--   { getLoc :: GetLoc,
--     spilled :: !Int
--   }