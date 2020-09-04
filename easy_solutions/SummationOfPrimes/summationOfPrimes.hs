{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

https://projecteuler.net/problem=10
-}

sieve :: (Integral a) => a -> [a]
sieve n = getMultiple [2..n]
    where getMultiple [] = []
          getMultiple [x] = [x]
          getMultiple (x:xs) =
              if x^2 <= n
              then x : getMultiple [y | y <- xs, y `mod` x /= 0]
              else x:xs

sumPrime :: (Integral a) => a -> a 
sumPrime n = sum $ takeWhile (<= n) (sieve n)

main :: IO ()
main = print $ sumPrime 2000000