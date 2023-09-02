import Data.List (foldl')

main :: IO ()
main = print (lcmA [1..20])

smallestMultipleSlow n = foldl' f 1 [2..n]
  where f acc m = minimum $ filterDivisibleBy m [1..acc * m]

filterDivisibleBy :: Integral a => a -> [a] -> [a]
filterDivisibleBy m = filter (\y -> all (\z -> y `mod` z == 0) [2..m])

smallestMultiple n = undefined

lcm' :: Integral a => a -> a -> a
lcm' a b = (a * b) `div` gcd a b

lcmA :: Integral a => [a] -> a
lcmA = foldl' lcm' 1
