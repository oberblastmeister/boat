module Oat.Main where
import qualified Oat.Opt as Opt
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = runEff run

run :: IOE :> es => Eff es ()
run = do
  opt <- Opt.opt
  pPrint opt
  pure ()