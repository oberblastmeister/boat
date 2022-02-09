{-# LANGUAGE UndecidableInstances #-}

module Control.Several where

import Effectful.State.Static.Local

type HList :: [Type] -> Type
data HList ts where
  HNil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

infixr 5 :::

type family TypeMap (f :: a -> b) (xs :: [a]) where
  TypeMap _ '[] = '[]
  TypeMap f (x ': xs) = f x ': TypeMap f xs

type family TypeAppend (a :: [t]) (b :: [t]) where
  TypeAppend '[] b = b
  TypeAppend (a ': as) b = a ': TypeAppend as b

runSeveral ::
  forall e t r a.
  (forall r' k x. k -> Eff (e k ': r') x -> Eff r' x) ->
  HList t ->
  Eff (TypeAppend (TypeMap e t) r) a ->
  Eff r a
runSeveral f (a ::: as) = runSeveral f as . f a
runSeveral _ HNil = id

testing :: '[State Int, State Char, State Bool] :>> es => Eff es ()
testing = do
  put 1
  put 'c'
  put True

another :: Eff es ((((), Int), Char), Bool)
another =
  testing
    & runState 1
    & runState 'c'
    & runState True

hlist :: HList '[Int, Char, Bool]
hlist = 1 ::: 'c' ::: True ::: HNil