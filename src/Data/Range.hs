{-# LANGUAGE UndecidableInstances #-}

module Data.Range
  ( Range (RangeP),
    length,
    unsafeNew,
    new,
    tuple,
    contains,
    empty,
    isEmpty,
    validateRange,
    withRange,
    setStart,
    setEnd,
  )
where

import Data.Data (Data)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Optics hiding (contains)
import Prelude hiding (empty, length)

data Range = Range
  { start :: !Int,
    end :: !Int
  }
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance LabelOptic "start" A_Lens Range Range Int Int where
  labelOptic = lens start (flip setStart)

instance LabelOptic "end" A_Lens Range Range Int Int where
  labelOptic = lens end (flip setEnd)

instance Semigroup Range where
  Range {start, end} <> Range {start = start', end = end'} =
    Range {start = min start start', end = max end end'}

instance Unbox Range

newtype instance VUM.MVector s Range = MV_Range (VU.MVector s (Int, Int))

newtype instance VU.Vector Range = V_Range (VU.Vector (Int, Int))

instance VM.MVector VU.MVector Range where
  basicLength (MV_Range v) = VM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i i' (MV_Range v) = coerce $ VM.basicUnsafeSlice i i' v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Range v) (MV_Range v') = VM.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew i = MV_Range <$> VM.basicUnsafeNew i
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Range v) = VM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_Range v) i = uncurry Range <$> VM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Range v) i x = VM.basicUnsafeWrite v i (x ^. tuple)
  {-# INLINE basicUnsafeWrite #-}

instance V.Vector VU.Vector Range where
  basicUnsafeFreeze (MV_Range v) = V_Range <$> V.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Range v) = MV_Range <$> V.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Range v) = V.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i i' (V_Range v) = V_Range $ V.basicUnsafeSlice i i' v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Range v) i = view (re tuple) <$> V.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}

pattern RangeP :: Int -> Int -> Range
pattern RangeP start end <- Range {start, end} where RangeP = new

{-# COMPLETE RangeP #-}

tuple :: Iso' Range (Int, Int)
tuple = iso (\Range {start, end} -> (start, end)) (\(start, end) -> Range {start, end})

unsafeNew :: Int -> Int -> Range
unsafeNew = Range

validateRange :: Text -> Range -> ()
validateRange name Range {start, end}
  | start > end = error $ prefix <> "start cannot be greater than end"
  | start < 0 = error $ prefix <> "start cannot be negative"
  | end < 0 = error $ prefix <> "end cannot be negative"
  | otherwise = ()
  where
    prefix = "Data.Range." <> name <> ": "

new :: Int -> Int -> Range
new start end = range
  where
    !_ = validateRange "new" range
    range = Range {start, end}

length :: Range -> Int
length Range {start, end} = end - start

contains :: Range -> Range -> Bool
contains Range {start, end} Range {start = start', end = end'} =
  start <= start' && end < end'

isEmpty :: Range -> Bool
isEmpty Range {start, end} = start == end

empty :: Int -> Range
empty i = Range i i

withRange :: Range -> (Range -> Range) -> Range
withRange range f = range'
  where
    !_ = validateRange "withRange" range
    range' = f range

setStart :: Int -> Range -> Range
setStart start' range = range'
  where
    !_ = validateRange "setStart" range'
    range' = range {start = start'}

setEnd :: Int -> Range -> Range
setEnd end' range = range'
  where
    !_ = validateRange "setEnd" range'
    range' = range {end = end'}