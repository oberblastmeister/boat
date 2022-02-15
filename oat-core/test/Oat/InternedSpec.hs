module Oat.InternedSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Instances ()

-- testInterned ::
--   forall a un.
--   ( un ~ Uninterned a,
--     Interned a,
--     Show a,
--     Arbitrary un,
--     Show un
--   ) =>
--   Proxy a ->
--   String ->
--   Spec
-- testInterned _p name = parallel $
--   describe name $ do
--     prop "Eq" $ \(un :: un) (un' :: un) ->
--       (un == un') == (intern @a un == intern @a un')

--     prop "Show" $ \(un :: un) (un' :: un) ->
--       (show @Text un == show @Text un') == (show @Text (intern @a un) == show @Text (intern @a un'))

--     prop "intern unintern refl" $ \(un :: un) ->
--       un == unintern (intern @a un)

spec :: Spec
spec = parallel $ do
  pure ()

-- testInterned (Proxy @IText) "IText"
-- testInterned (Proxy @IShortByteString) "IShortByteString"
