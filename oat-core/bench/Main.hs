module Main where

import Control.Parallel.Strategies qualified as Parallel
import Criterion (bench, bgroup, nf, whnf)
import Criterion qualified
import Criterion.Main qualified as Criterion
import Oat.Common (parOver)

data ListMap a b s t = ListMap String ((a -> b) -> s -> t)

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup "parMapBenches" parMapBenches
    ]

parMapBenches :: [Criterion.Benchmark]
parMapBenches =
  listMaps
    [ ListMap "map" map,
      ListMap "parMap" (Parallel.parMap Parallel.rseq),
      ListMap "parOver" (parOver Parallel.rseq traversed)
    ]
    ++ [ bench
           "parOver tuple"
           ( nf
               (parOver Parallel.rseq (traversed % _1) fib)
               (replicate 10 (30 :: Integer, 30 :: Integer))
           )
       ]
  where
    listMaps funcs =
      [ bench
          (title ++ ": " ++ show n)
          ( nf
              (func fib)
              (replicate n (30 :: Integer))
          )
        | n <- [10 :: Int],
          ListMap title func <- funcs
      ]

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)