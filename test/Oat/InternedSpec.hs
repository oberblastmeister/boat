module Oat.InternedSpec (spec) where

import Oat.Interned
import Oat.Interned.ShortByteString
import Oat.Interned.Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = parallel $ do
  describe "interned Text" $ do
    parallel $ do
      prop "Eq" $ \(t :: Text) (t' :: Text) ->
        (t == t') == (fromText t == fromText t')

      prop "unintern" $ \(t :: Text) ->
        t == unintern (intern @IText t)

  describe "interned ShortByteString" $ do
    parallel $ do
      prop "Eq" $ \(b :: ShortByteString) (b' :: ShortByteString) ->
        (b == b') == (fromShortByteString b == fromShortByteString b')

      prop "unintern" $ \(t :: ShortByteString) ->
        t == unintern (intern @IShortByteString t)