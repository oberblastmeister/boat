{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.SelfField
  ( SelfField (..),
  )
where

newtype SelfField name a = SelfField {unSelfField :: a}

makeFieldLabelsNoPrefix ''SelfField

instance LabelOptic name An_Iso (SelfField name a) (SelfField name a) a a where
  labelOptic = #unSelfField