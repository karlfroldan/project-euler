import Data.List (nub)
import Data.Maybe (maybeToList)

-- Clearly, this method is super duper inefficient. Maybe use an array next?
primes :: [Integer]
primes = primes' [2 ..]
  where
    primes' (x : xs) = x : primes' (filter (\z -> z `mod` x /= 0) xs)

primeFactors :: Integer -> [Integer]
primeFactors z = (filter (\p -> z `mod` p == 0) . takeWhile (< squareRoot z)) primes

main :: IO ()
-- My implementation seems to miss some primes so we use trial division
-- once the number is made smaller by pollardRho
main = print (maximum $ nub $ primeFactors withRhoFactors ++ rhoFactors)
  where
    rhoFactors = primeFactors' (n :: Integer)
    n = 600851475143 :: Integer
    withRhoFactors = foldl (\acc p -> removePrimeFactor n p) n rhoFactors

-- From Haskell Wiki
(^!) :: (Num a) => a -> Int -> a
(^!) x n = x ^ n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
    let twopows = iterate (^! 2) 2
        (lowerRoot, lowerN) =
            last $ takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
        newtonStep x = div (x + div n x) 2
        iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
        isRoot r = r ^! 2 <= n && n < (r + 1) ^! 2
     in head $ dropWhile (not . isRoot) iters

pollardRho :: (Integral a) => a -> Maybe a
pollardRho = pollardRho' 2 2 1

pollardRho' x y d n
    | d' == 1 = pollardRho' x' y' d' n
    | d' == n = Nothing
    | otherwise = Just d'
  where
    x' = g1 x n
    y' = g1 (g1 y n) n
    d' = gcd (abs (x' - y')) n

primeFactors' :: (Integral a) => a -> [a]
primeFactors' n = case pollardRho n of
    Nothing -> []
    Just p -> p : primeFactors' (removePrimeFactor n p)

removePrimeFactor :: (Integral a) => a -> a -> a
removePrimeFactor n p
    | n `mod` p == 0 = removePrimeFactor (n `div` p) p
    | otherwise = n

g1 :: (Integral a) => a -> a -> a
g1 x n = (x2 + 1) `mod` n
  where
    x2 = x * x
