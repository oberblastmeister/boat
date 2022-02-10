module Oat.Driver where

import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.Temporary (Temporary)
import Effectful.Temporary qualified as Temporary
import Oat.Backend.X86.Codegen qualified as Backend.X86.Codegen
import Oat.Backend.X86.Pretty qualified as Backend.X86.Pretty
import Oat.Backend.X86.X86 qualified as Backend.X86
import Oat.Command (Command)
import Oat.Command qualified as Command
import Oat.Common (liftEither, readFileUtf8, writeFileUtf8)
import Oat.LL qualified as LL
import Oat.LL.ParserWrapper qualified as LL.ParserWrapper
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as Prettyprinter

parseLL :: (Error [LL.ParseError] :> es) => Text -> Eff es LL.Prog
parseLL text = liftEither $ LL.parse text LL.prog

parseLLFile ::
  '[ Error UnicodeException,
     Error [LL.ParseError],
     IOE
   ]
    :>> es =>
  FilePath ->
  Eff es LL.Prog
parseLLFile path = readFileUtf8 path >>= parseLL

compileLLFile ::
  '[ Error UnicodeException,
     Error [LL.ParseError],
     Temporary,
     IOE,
     Command
   ]
    :>> es =>
  FilePath ->
  FilePath ->
  Eff es ()
compileLLFile dotLL dotO = do
  llText <- readFileUtf8 dotLL
  compileLLTextToFile llText dotO

compileLLTextToFile ::
  '[ Error UnicodeException,
     Error [LL.ParseError],
     Temporary,
     IOE,
     Command
   ]
    :>> es =>
  Text ->
  FilePath ->
  Eff es ()
compileLLTextToFile llText dotO = do
  dotSText <- compileLLText llText
  Temporary.withSystemTempFile "oat" $ \dotSTemp _ -> do
    writeFileUtf8 dotSTemp dotSText
    compileAsm dotSTemp dotO

compileLLText :: '[Error [LL.ParseError]] :>> es => Text -> Eff es Text
compileLLText text = do
  insts <- compileLLTextToInsts text
  let prog = Backend.X86.instLabToElems $ toList insts
  let asmDoc = Backend.X86.Pretty.prettyProg prog
  let asmText = Prettyprinter.renderStrict $ Prettyprinter.layoutCompact asmDoc
  pure asmText

compileLLTextToInsts :: '[Error [LL.ParseError]] :>> es => Text -> Eff es (Seq Backend.X86.InstLab)
compileLLTextToInsts text = do
  insts <- parseLL text
  LL.runNameSource $ Backend.X86.Codegen.compileProg insts

compileAsm :: '[Command, Temporary] :>> es => FilePath -> FilePath -> Eff es ()
compileAsm dotS dotO = do
  Temporary.withSystemTempFile "oat" $ \dotOTemp _ -> do
    Command.assemble dotS dotOTemp
    Command.link [dotOTemp] dotO

compileLLFileClang :: '[Command, Temporary] :>> es => FilePath -> FilePath -> Eff es ()
compileLLFileClang dotLL dotO = do
  Temporary.withSystemTempFile "oat" $ \dotSTemp _ -> do
    Command.compileLlvm dotLL dotSTemp
    compileAsm dotSTemp dotO
