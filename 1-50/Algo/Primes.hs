module Algo.Primes where

-- \| Prime sieve implementation from
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
--
-- as stated in the paper, we need to be able to generate
-- an infinite list of primes... rather, vectors.

import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as M
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as PQ

badPrimes = badSieve [2 ..]
  where
    badSieve (p : xs) = p : badSieve [x | x <- xs, x `mod` p > 0]

sieve1 :: (Ord a, Num a) => [a] -> [a]
sieve1 xs = sieve' xs M.empty
  where
    sieve' [] table = []
    sieve' (x : xs) table =
        case M.lookup x table of
            -- If the iterator is not yet found in the lookup table,
            -- insert p^2 to the lookup table.
            Nothing -> x : sieve' xs (M.insert (x * x) [x] table)
            -- Otherwise, increment the iterator.
            Just factors -> sieve' xs (foldl' reinsert (M.delete x table) factors)
      where
        reinsert table prime = M.insertWith (++) (x + prime) [prime] table

deleteMinAndInsert :: Ord k => k -> v -> MinPQueue k v -> MinPQueue k v
deleteMinAndInsert k v = PQ.insert k v . PQ.deleteMin

primes = sieve1 [2 ..]
