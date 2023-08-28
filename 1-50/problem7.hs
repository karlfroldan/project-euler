import Algo.Primes

main :: IO ()
main = print (primes !! (idx - 1))
  where
    idx = 10001
