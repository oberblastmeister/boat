module Oat.Driver where

import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import Effectful.Error.Static
import Effectful.Temporary (Temporary)
import Effectful.Temporary qualified as Temporary
import Named (NamedF (Arg), arg, argDef, argF, (!), type (:!), type (:?))
import Oat.Backend.X86.Codegen qualified as Backend.X86.Codegen
import Oat.Backend.X86.X86 qualified as Backend.X86
import Oat.Command (Command)
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

compileAsmFile :: '[Command, Temporary] :>> es => FilePath -> FilePath -> Eff es ()
compileAsmFile dotS dotO = do
  Temporary.withSystemTempFile "oat" $ \dotOTemp _handle -> do
    Command.assemble dotS dotOTemp
    Command.link [dotOTemp] dotO

compileLLFileClang :: '[Command, Temporary] :>> es => FilePath -> FilePath -> Eff es ()
compileLLFileClang dotLL dotO = do
  Temporary.withSystemTempFile "oat" $ \dotSTemp _handle -> do
    Command.compileLlvm dotLL dotSTemp
    compileAsmFile dotSTemp dotO