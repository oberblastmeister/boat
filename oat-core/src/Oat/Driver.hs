module Oat.Driver where

import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import Effectful.Error.Static
import Named (NamedF (Arg), arg, argDef, argF, (!), type (:!), type (:?))
import Oat.Backend.X86.Codegen qualified as Backend.X86.Codegen
import Oat.Backend.X86.X86 qualified as Backend.X86
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

compileLLTextToInsts :: '[Error [LL.ParseError]] :>> es => Text -> Eff es (Seq Backend.X86.InstLab)
compileLLTextToInsts text = do
  prog <- parseLL text
  LL.runNameSource $ Backend.X86.Codegen.compileProg prog