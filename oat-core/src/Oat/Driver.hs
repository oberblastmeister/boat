module Oat.Driver where

import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import Effectful.Error.Static
import Oat.Command qualified as Command
import Oat.Common (liftEither, readFileUtf8)
import Oat.LL qualified as LL
import Oat.LL.ParserWrapper qualified as LL.ParserWrapper

parseLL :: (Error [LL.ParseError] :> es) => Text -> Eff es LL.Prog
parseLL text = liftEither $ LL.parse text LL.prog

parseLLFile ::
  '[ Error Text.Encoding.Error.UnicodeException,
     Error [LL.ParseError],
     IOE
   ]
    :>> es =>
  FilePath ->
  Eff es LL.Prog
parseLLFile path = readFileUtf8 path >>= parseLL
