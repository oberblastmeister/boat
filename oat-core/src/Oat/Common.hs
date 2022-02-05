{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Oat.Common
  ( internalError,
    unwrap,
    unreachable,
    hashSetOf,
    makeFieldGetterLabelsNoPrefix,
    inBetween,
    insOrdSetOf,
    swap,
    ShowableException,
    runErrorIO,
    liftEither,
  )
where

import Control.Exception.Safe qualified as Exception
import Data.HashSet qualified as HashSet
import Data.HashSet.InsOrd (InsOrdHashSet)
import Data.HashSet.InsOrd qualified as InsOrdHashSet
import Data.IntMap qualified as IntMap
import Data.Range (Range (RangeP))
import Effectful.Error.Static (Error, runError, throwError)
import Language.Haskell.TH qualified as TH
import Prelude hiding (Map)

internalError :: forall a. HasCallStack => Text -> a
internalError t = error $ "Internal compiler error: " <> t

-- non but only a lens
unwrap :: a -> Lens' (Maybe a) a
unwrap def =
  lens
    (fromMaybe def)
    ( \m b -> case m of
        Just _ -> Just b
        Nothing -> Just def
    )

unreachable :: forall a. a
unreachable = error "Unreachable!"

hashSetOf :: (Eq a, Hashable a, Is k A_Fold) => Optic' k is s a -> s -> HashSet a
hashSetOf fold = foldMapOf fold HashSet.singleton

makeFieldGetterLabelsNoPrefix :: TH.Name -> TH.DecsQ
makeFieldGetterLabelsNoPrefix =
  makeFieldLabelsWith $
    noPrefixFieldLabels
      & generateUpdateableOptics .~ False

inBetween :: Range -> IntMap a -> IntMap a
inBetween (RangeP start end) imap = imap''
  where
    (imap'', _) = IntMap.split end imap'
    (_, imap') = IntMap.split start imap

insOrdSetOf :: (Is k A_Fold, Eq a, Hashable a) => Optic' k is s a -> s -> InsOrdHashSet a
insOrdSetOf o = foldlOf' o (flip InsOrdHashSet.insert) mempty

swap :: (Is k An_AffineFold, Is k' A_Review) => Optic' k is t a -> Optic' k' is' t b -> Setter t t a b
swap o o' = sets $ \f x -> case x ^? o of
  Nothing -> x
  Just y -> o' # f y

data ShowableException = forall e. Show e => ShowableException e
  deriving (Typeable)

instance Exception.Exception ShowableException

instance Show ShowableException where
  show (ShowableException e) = show e

runErrorIO :: forall e es a. (Show e) => Eff (Error e ': es) a -> Eff es a
runErrorIO m = do
  res <- runError m
  case res of
    Left e -> Exception.throw $ ShowableException e
    Right a -> pure a

liftEither :: (Error e :> es) => Either e a -> Eff es a
liftEither (Left e) = throwError e
liftEither (Right a) = pure a