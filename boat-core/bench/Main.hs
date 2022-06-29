module Main where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Parallel.Strategies qualified as Parallel
import Criterion (bench, bgroup, nf, nfIO, whnf)
import Criterion qualified
import Criterion.Main qualified as Criterion
import Boat.Common (parOver)

data ListMap a b s t = ListMap String ((a -> b) -> s -> t)

main :: IO ()
main =
  Criterion.defaultMain
    [ -- bgroup "parMapBenches" parMapBenches,
      bgroup "exceptionBenches" exceptionBenches
    ]

exceptionBenches :: [Criterion.Benchmark]
exceptionBenches =
  [ bench
      "either"
      (nf eitherLoop 100),
    bench
      "exception"
      (nfIO $ Exception.catch @Exception.SomeException (exceptionLoop 100) (\_ -> pure ()))
  ]

eitherLoop :: Int -> Either Int ()
eitherLoop 0 = Left 1
eitherLoop n = do
  case eitherLoop $ n - 1 of
    Left res -> Left $ res + 10
    other -> other
{-# NOINLINE eitherLoop #-}

data Oops = Oops Int
  deriving (Show, Typeable)

instance Exception Oops

exceptionLoop :: Int -> IO ()
exceptionLoop 0 = Exception.throwIO $ Oops 1
exceptionLoop n = do
  Exception.catch
    (exceptionLoop $ n - 1)
    (\(Oops res) -> Exception.throwIO $ Oops $ res + 10)
{-# NOINLINE exceptionLoop #-}

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
