module Oat.Driver where

import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import Effectful.Error.Static
import Named (NamedF (Arg), arg, argDef, argF, (!), type (:!), type (:?))
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

testing :: "first" :! Int -> "second" :! (Int -> Bool) -> "third" :! Char -> "f" :! (Int -> Char) -> Bool
testing (Arg first) (Arg second) (Arg third) (Arg f) = second first

testing' :: ("first" :! Int) -> ("second" :! (Int -> Bool)) -> ("third" :! Char) -> Bool
testing' = testing ! #f (\i -> 'x')

testing'' :: ("first" :! Int) -> ("third" :! Char) -> Bool
testing'' = testing' ! #second (\i -> True)

-- testing' :: "second" :! (Int -> Bool) -> "third" :! Char -> "f" :! (Int -> Char) -> "optional" :? Text -> Bool
-- testing' (argDef #optional "x" -> optional) = undefined

-- testing''' :: "second" :! (Int -> Bool) -> "third" :! Char -> "f" :! (Int -> Char) -> "optional" :? Text -> Bool
-- testing''' :: "optional" :? Char -> Bool
-- testing''' (argDef #optional 'c' -> c) = undefined

-- testing'' :: Bool
-- testing'' =
--   testing'
--     ! #second (\i -> True)
--     ! #f (\i -> 'x')
--     ! #third 'x'
--     ! #optional "x"
