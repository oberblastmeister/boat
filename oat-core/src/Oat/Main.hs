module Oat.Main where
import qualified Oat.Opt as Opt

main :: IO ()
main = runEff run

run :: IOE :> es => Eff es ()
run = do
  opt <- Opt.opt
  pure ()